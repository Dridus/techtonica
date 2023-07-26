module Tech.Pretty where

import Control.Lens (each, maximumOf, over, to, toListOf, view, _1, _2, _3)
import Data.Fixed (Fixed, HasResolution, Milli, showFixed)
import Data.Graph.Inductive (Graph, LEdge, LNode, Node, context, gsel, labEdges, leveln, nodes)
import Data.Map.Strict qualified as Map
import Data.Sequence ((|>))
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.RealFloat as TLBRF
import Prettyprinter (
  Doc,
  annotate,
  comma,
  enclose,
  hsep,
  indent,
  lbrace,
  lparen,
  pretty,
  punctuate,
  rbrace,
  rparen,
  viaShow,
  vsep,
  (<+>),
 )
import Prettyprinter.Internal (unsafeTextWithoutNewlines)
import Prettyprinter.Render.Terminal (
  AnsiStyle,
  Color (Black, Blue, Cyan, Green, Magenta, Red, White, Yellow),
  color,
  colorDull,
 )
import Tech.Planner (nExternalSink, nExternalSource)
import Tech.Store (InstantiateError, LoadError, LoadWarning)
import Tech.Store qualified as Store
import Tech.Types
import Tech.Verify (VerifyError, VerifyWarning)
import Tech.Verify qualified as Verify

kw :: Doc AnsiStyle -> Doc AnsiStyle
kw = annotate (color Magenta)

parens :: Doc AnsiStyle -> Doc AnsiStyle
parens =
  enclose
    (annotate (color Black) lparen)
    (annotate (color Black) rparen)

braces :: Doc AnsiStyle -> Doc AnsiStyle
braces =
  enclose
    (annotate (color Black) lbrace)
    (annotate (color Black) rbrace)

ppRealFloat :: forall a ann. RealFloat a => TLBRF.FPFormat -> Maybe Int -> a -> Doc ann
ppRealFloat = (fmap . fmap) (pretty . TLB.toLazyText) . TLBRF.formatRealFloat

ppRealFloatMilli :: forall a ann. RealFloat a => a -> Doc ann
ppRealFloatMilli = ppRealFloat TLBRF.Fixed (Just 3)

ppItem :: Item -> Doc AnsiStyle
ppItem = annotate (color White) . pretty . unItem

ppMachine :: Machine -> Doc AnsiStyle
ppMachine = annotate (colorDull Cyan) . pretty . unMachine

ppRecipeIdentifier :: RecipeIdentifier -> Doc AnsiStyle
ppRecipeIdentifier = pretty . unRecipeIdentifier

ppRational :: Rational -> Doc ann
ppRational ratio =
  pretty (numerator ratio) <> if denominator ratio == 1 then mempty else "/" <> pretty (denominator ratio)

ppFixed :: HasResolution a => Bool -> Fixed a -> Doc ann
ppFixed chopZeroes = unsafeTextWithoutNewlines . pack . showFixed chopZeroes

ppQuantity :: Quantity -> Doc AnsiStyle
ppQuantity = annotate (colorDull Blue) . ppRational . unQuantity

ppRate :: Rate -> Doc AnsiStyle
ppRate r =
  (annotate (color Blue) . ppFixed False . realToFrac @Rational @Milli . (* 60) . unRate $ r)
    <> annotate (color Black) "/min"

ppImage :: (q -> Doc AnsiStyle) -> Image q -> Doc AnsiStyle
ppImage ppQ im =
  case pairs of
    [] -> "()"
    [p] -> ppPair p
    _ -> braces . hsep . punctuate (annotate (color Black) comma) . map ppPair $ pairs
 where
  pairs = Map.toList im
  ppPair (i, q) = ppQ q <+> ppItem i

ppTransfer :: (q -> Doc AnsiStyle) -> Transfer q -> Doc AnsiStyle
ppTransfer ppQ (Transfer ins outs) = ppImage ppQ ins <+> ">->" <+> ppImage ppQ outs

ppRecipeKey :: RecipeKey -> Doc AnsiStyle
ppRecipeKey rk =
  ppMachine (view machine rk) <> annotate (color Black) "/" <> ppRecipeIdentifier (view identifier rk)

ppRecipe :: Recipe -> Doc AnsiStyle
ppRecipe r =
  ppRecipeKey (view key r)
    <+> kw "recipe"
    <+> ppRecipeFunction r

ppRecipeFunction :: Recipe -> Doc AnsiStyle
ppRecipeFunction r =
  annotate
    (colorDull White)
    ( parens $
        (ppRealFloatMilli @Double . realToFrac . view cycleTime $ r)
          <> annotate (color Black) "sec/cycle"
    )
    <+> ppTransfer ppQuantity (view transfer r)

ppAnonymousBeltSt :: BeltSt -> Doc AnsiStyle
ppAnonymousBeltSt b =
  kw "belt"
    <+> "carrying"
    <+> ppItem (view item b)

ppBeltKey :: (Node, Node) -> Doc AnsiStyle
ppBeltKey (np, ns) =
  annotate (color Yellow) (if np == nExternalSource then "external" else pretty np)
    <+> "↘"
    <+> annotate (color Yellow) (if ns == nExternalSink then "external" else pretty ns)
    <> annotate (color Black) (pretty ':')

ppBeltSt :: (Node, Node) -> BeltSt -> Doc AnsiStyle
ppBeltSt k c = ppBeltKey k <+> ppAnonymousBeltSt c

ppAnonymousBeltDy :: BeltDy -> Doc AnsiStyle
ppAnonymousBeltDy b =
  kw "belt"
    <+> "carrying"
    <+> ppItem (view item b)
    <+> parens
      ( ppRate (view entering b)
          <+> ( case compare (view entering b) (view exiting b) of
                  LT ->
                    ">-"
                      <+> annotate (colorDull Red) ("short" <+> ppRate (shortfall b))
                      <+> "->"
                  EQ -> annotate (colorDull Green) ">->"
                  GT ->
                    ">-"
                      <+> annotate (colorDull Yellow) ("over" <+> ppRate (overflow b))
                      <+> "->"
              )
          <+> ppRate (view exiting b)
      )

ppBeltDy :: (Node, Node) -> BeltDy -> Doc AnsiStyle
ppBeltDy k c = ppBeltKey k <+> ppAnonymousBeltDy c

ppAnonymousClusterSt :: ClusterSt -> Doc AnsiStyle
ppAnonymousClusterSt c =
  kw "cluster"
    <+> "of"
    <+> ppQuantity (view quantity c)
    <+> ppRecipeKey (view (recipe . key) c)
      <> ":"
    <+> ppRecipeFunction (view recipe c)

ppClusterSt :: Node -> ClusterSt -> Doc AnsiStyle
ppClusterSt n c =
  annotate (color Yellow) (pretty n)
    <> annotate (color Black) (pretty ':')
    <+> ppAnonymousClusterSt c

ppAnonymousClusterDy :: ClusterDy -> Doc AnsiStyle
ppAnonymousClusterDy c =
  kw "cluster"
    <+> "of"
    <+> ppQuantity (view quantity c)
    <+> ppRecipeKey (view (recipe . key) c)
      <> ":"
    <+> ppTransfer ppRate (view transfer c)

ppClusterDy :: Node -> ClusterDy -> Doc AnsiStyle
ppClusterDy n c
  | n == nExternalSource =
      kw "external resources"
        <+> "↓"
        <+> ppImage ppRate (view (transfer . outputs) c)
  | n == nExternalSink =
      kw "byproducts"
        <+> "↓"
        <+> ppImage ppRate (view (transfer . inputs) c)
  | otherwise =
      annotate (color Yellow) (pretty n)
        <> annotate (color Black) (pretty ':')
        <+> ppAnonymousClusterDy c

coNodeLine :: String
coNodeLine = repeat '━'
coEdgeLine :: String
coEdgeLine = repeat '┄'
emptyLine :: String
emptyLine = repeat ' '

ppGraph
  :: forall a b bk g
   . (Graph g, Ord bk, Show bk)
  => (Node -> a -> Doc AnsiStyle)
  -> (b -> bk)
  -> ((Node, Node) -> b -> Doc AnsiStyle)
  -> g a b
  -> Doc AnsiStyle
ppGraph ppNode edgeKey ppEdge g =
  render . over _2 padTracks . generate . map fst $ leveln ((,0) <$> roots) g
 where
  roots :: [Node]
  roots = toListOf (each . _2) $ gsel (\(pre, _, _, _) -> null pre) g

  -- Initial markers that would terminate in a supposed root node, except that we should have
  -- sorted the graph by now oriented at the roots, indicating the graph structure is not
  -- sound.
  danglingInitialMarkers :: Map (Node, Node, bk) Int
  danglingInitialMarkers =
    let validNodes = Set.fromList (nodes g)
    in  Map.fromList
          . flip zip [0 ..]
          . fmap (over _3 edgeKey)
          . filter (\(np, _, _) -> not (Set.member np validNodes))
          . labEdges
          $ g

  -- The final conversion from row data and dangling markers that never terminated into a Doc.
  render :: (Map (Node, Node, bk) Int, Seq (Text, Either (LNode a) (LEdge b))) -> Doc AnsiStyle
  render (danglingTrailingMarkers, rows) =
    vsep . concat @[] $
      [ [ vsep
          ( annotate (color Red) "dangling predecessor edges:"
              : fmap
                (\(np, ns, bk) -> ppBeltKey (np, ns) <+> viaShow bk)
                (Map.keys danglingInitialMarkers)
          )
        | not (Map.null danglingTrailingMarkers)
        ]
      , map
          ( \(track, row) ->
              pretty track <+> case row of
                Left (n, a) -> ppNode n a
                Right (np, ns, b) -> ppEdge (np, ns) b
          )
          (toList rows)
      , [ vsep
          ( annotate (color Red) "dangling successor edges:"
              : fmap
                (\(np, ns, bk) -> ppBeltKey (np, ns) <+> viaShow bk)
                (Map.keys danglingTrailingMarkers)
          )
        | not (Map.null danglingTrailingMarkers)
        ]
      ]

  -- Goes through the row data and repeats the trailing character of each track shorter than
  -- the longest until all are of equal length
  padTracks :: Seq (Text, Either (LNode a) (LEdge b)) -> Seq (Text, Either (LNode a) (LEdge b))
  padTracks rows = fmap f rows
   where
    longest = fromMaybe 0 $ maximumOf (each . _1 . to T.length) rows
    padTo :: Int -> Text -> Text
    padTo c t = t <> (T.pack . replicate (c - T.length t) . maybe 'x' snd . T.unsnoc $ t)
    f (track, rhs@(Left _)) = (padTo longest track, rhs)
    f (track, rhs@(Right _)) = (padTo (longest + 2) track, rhs)

  -- Step through the nodes of the graph which have already been sorted, generating row data
  -- for each node and successor edge visited. Row data consists of the left graph structure
  -- rendering and either a node or edge to display.
  --
  -- Because the nodes are sorted and the graph should be a DAG, this should visit every node
  -- and edge in the graph, however if the graph is not a DAG or otherwise unsound then the
  -- excess unterminated markers are returned for rendering.
  generate :: [Node] -> (Map (Node, Node, bk) Int, Seq (Text, Either (LNode a) (LEdge b)))
  generate = foldl' f (danglingInitialMarkers, mempty)
   where
    f
      :: (Map (Node, Node, bk) Int, Seq (Text, Either (LNode a) (LEdge b)))
      -> Node
      -> (Map (Node, Node, bk) Int, Seq (Text, Either (LNode a) (LEdge b)))
    f (markersAbove, rows0) n =
      let
        (pre, _, a, suc) = context g n

        -- which marker tracks are terminating at this node from above
        terminating :: Map (Node, Node, bk) Int
        terminating = Map.fromList $ pre <&> \(b, np) -> ((np, n, edgeKey b), 0 {- key ignored -})

        -- the set of markers after removing the ones terminating at this node
        survivingMarkers :: Map (Node, Node, bk) Int
        survivingMarkers = Map.difference markersAbove terminating

        -- which marker tracks are starting from this node towards below
        introducing :: Map (Node, Node, bk) (b, Int)
        introducing =
          -- allocate columns for markers by going from left to right looking for a free one,
          view _3 $
            foldl'
              ( \(c0, occupied, m) (b, ns) ->
                  let c' = flip fix c0 $ \continue c -> if Set.member c occupied then continue (succ c) else c
                  in  ( c'
                      , Set.insert c' occupied
                      , Map.insert (n, ns, edgeKey b) (b, c') m
                      )
              )
              (0, Set.fromList (Map.elems survivingMarkers), mempty)
              suc

        addIntroducing = Map.union (snd <$> introducing)
        allMarkersAtRow = addIntroducing markersAbove
        markersBelow = addIntroducing survivingMarkers

        track
          :: String
          -> (Bool -> NonEmpty (Node, Node, bk) -> Maybe Char)
          -> Map (Node, Node, bk) Int
          -> Text
        track coLine hitf =
          TL.toStrict
            . TLB.toLazyText
            . (\(_, co, tb) -> tb <> TLB.fromString (take 1 (if co then coLine else emptyLine)))
            . renderColumns
              (0, False, mempty)
            . sortOn snd
            . Map.toList
         where
          renderColumns :: (Int, Bool, TLB.Builder) -> [((Node, Node, bk), Int)] -> (Int, Bool, TLB.Builder)
          renderColumns s [] = s
          renderColumns (c, co, tb) (firstHit@(_, c') : rest) =
            let
              (hits, rest') = over _1 (firstHit :|) (span ((== c') . snd) rest)
              hit = hitf co (fst <$> hits)
            in
              renderColumns
                ( c'
                , co || isJust hit
                , tb
                    <> TLB.fromString (take ((c' - c) * 2) (if co then coLine else emptyLine))
                    <> TLB.singleton (fromMaybe '┃' hit)
                )
                rest'

        nodeTrack = track coNodeLine nodeTrackHit allMarkersAtRow
        nodeTrackHit co ks = case (starting, ending, co) of
          (True, True, True) -> Just '╋'
          (True, True, False) -> Just '┣'
          (True, False, True) -> Just '┳'
          (True, False, False) -> Just '┏'
          (False, True, True) -> Just '┻'
          (False, True, False) -> Just '┗'
          (False, False, True) -> Just '━'
          (False, False, False) -> Nothing
         where
          starting = any (`Map.member` introducing) ks
          ending = any (`Map.member` terminating) ks

        edgeTrack k' = track coEdgeLine (edgeTrackHit k') markersBelow
        edgeTrackHit k' _ ks = if k' `elem` ks then Just '┠' else Nothing
      in
        ( markersBelow
        , foldl'
            (\rows (k@(np, ns, _), (b, _)) -> rows |> (edgeTrack k, Right (np, ns, b)))
            (rows0 |> (nodeTrack, Left (n, a)))
            (reverse . Map.toList $ introducing)
        )

ppFactorySt :: FactorySt -> Doc AnsiStyle
ppFactorySt = ppGraph ppClusterSt (view item) ppBeltSt

ppFactoryDy :: FactoryDy -> Doc AnsiStyle
ppFactoryDy = ppGraph ppClusterDy (view item) ppBeltDy

ppVerifyError :: VerifyError -> Doc AnsiStyle
ppVerifyError = \case
  Verify.BeltUpNodeInvalid (np, ns, b) ->
    vsep
      [ annotate (color Red) ("Error: Belt upstream (predecessor) node" <+> pretty np <+> "invalid.")
      , indent 2 (ppBeltSt (np, ns) b)
      ]
  Verify.BeltDownNodeInvalid (np, ns, b) ->
    vsep
      [ annotate (color Red) ("Error: Belt downstream (successor) node" <+> pretty ns <+> "invalid.")
      , indent 2 (ppBeltSt (np, ns) b)
      ]

ppVerifyWarning :: VerifyWarning -> Doc AnsiStyle
ppVerifyWarning = \case
  Verify.BeltItemNotInputForDown (_, cp) (np, ns, b) ->
    vsep
      [ annotate (color Yellow) "Warning: Belt upstream recipe does not produce requested item."
      , indent 2 (vsep [ppBeltSt (np, ns) b, ppClusterSt np cp])
      ]
  Verify.BeltItemNotOutputByUp (_, cs) (np, ns, b) ->
    vsep
      [ annotate (color Yellow) "Warning: Belt downstream recipe does not accept requested item."
      , indent 2 (vsep [ppBeltSt (np, ns) b, ppClusterSt ns cs])
      ]

ppInstantiateError :: InstantiateError -> Doc AnsiStyle
ppInstantiateError = \case
  Store.UnrecognizedRecipeIdentifier rk ->
    annotate (color Red) ("Error: unknown recipe" <+> ppRecipeKey rk)

ppLoadError :: LoadError -> Doc AnsiStyle
ppLoadError = \case
  Store.ParseError parseEx ->
    annotate (color Red) (viaShow parseEx)
  Store.InstantiateError instErrs ->
    vsep $ ppInstantiateError <$> Set.toList instErrs
  Store.VerifyError (errs, warns) ->
    vsep $ (ppVerifyWarning <$> Set.toList warns) <> (ppVerifyError <$> Set.toList errs)

ppLoadWarning :: LoadWarning -> Doc AnsiStyle
ppLoadWarning = \case
  Store.VerifyWarning warn ->
    ppVerifyWarning warn

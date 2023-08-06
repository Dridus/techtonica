module Tech.Pretty where

import Control.Lens (each, maximumOf, over, to, toListOf, view, _1, _2, _3)
import Data.Fixed (Fixed, HasResolution, Milli, showFixed)
import Data.Graph.Inductive (DynGraph, Graph, LEdge, LNode, Node, components, context, gsel, labEdges, leveln, nodes, subgraph)
import Data.Map.Strict qualified as Map
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.RealFloat as TLBRF
import Prettyprinter (
  Doc,
  annotate,
  enclose,
  hang,
  indent,
  line,
  pretty,
  softline,
  viaShow,
  vsep,
  (<+>),
 )
import Prettyprinter qualified as Pp
import Prettyprinter.Internal (unsafeTextWithoutNewlines)
import Prettyprinter.Render.Terminal (
  AnsiStyle,
  Color (Black, Blue, Cyan, Green, Magenta, Red, White, Yellow),
  color,
  colorDull,
 )
import Tech.Machines (externalSink, externalSource)
import Tech.Planner.Propose (FactoryProp, IsNew (New), Proposal, ProposalError, ProposalStep, fFactory, fResult, fSteps)
import Tech.Planner.Propose qualified as Propose
import Tech.Store (InstantiateError, LoadError, LoadWarning)
import Tech.Store qualified as Store
import Tech.Types
import Tech.Verify (VerifyError, VerifyWarning)
import Tech.Verify qualified as Verify

kw :: Doc AnsiStyle -> Doc AnsiStyle
kw = annotate (color Magenta)

errDoc :: Doc AnsiStyle -> Doc AnsiStyle
errDoc = annotate (color Red)

warnDoc :: Doc AnsiStyle -> Doc AnsiStyle
warnDoc = annotate (color Yellow)

comma, lparen, rparen, lbrace, rbrace, lbracket, rbracket :: Doc AnsiStyle
comma = annotate (color Black) Pp.comma
lparen = annotate (color Black) Pp.lparen
rparen = annotate (color Black) Pp.rparen
lbrace = annotate (color Black) Pp.lbrace
rbrace = annotate (color Black) Pp.rbrace
lbracket = annotate (color Black) Pp.lbracket
rbracket = annotate (color Black) Pp.rbracket

encloseSep :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseSep l r s ds = l <> mconcat (intersperse (s <> " ") ds) <> r

parens :: Doc AnsiStyle -> Doc AnsiStyle
parens = enclose lparen rparen

parenthetical :: [Doc AnsiStyle] -> Doc AnsiStyle
parenthetical = encloseSep lparen rparen comma

braces :: Doc AnsiStyle -> Doc AnsiStyle
braces = enclose lbrace rbrace

braced :: [Doc AnsiStyle] -> Doc AnsiStyle
braced = encloseSep lbrace rbrace comma

brackets :: Doc AnsiStyle -> Doc AnsiStyle
brackets = enclose lbracket rbracket

bracketed :: [Doc AnsiStyle] -> Doc AnsiStyle
bracketed = encloseSep lbracket rbracket comma

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

ppPerMinute :: PerMinute -> Doc AnsiStyle
ppPerMinute r =
  (annotate (color Blue) . ppFixed False . realToFrac @Rational @Milli . unPerMinute $ r)
    <> annotate (color Black) "/min"

ppImage :: (q -> Doc AnsiStyle) -> Image q -> Doc AnsiStyle
ppImage ppQ im =
  case pairs of
    [] -> "()"
    [p] -> ppPair p
    _ -> braced (ppPair <$> pairs)
 where
  pairs = Map.toList im
  ppPair (i, q) = ppQ q <+> ppItem i

ppTransfer :: (q -> Doc AnsiStyle) -> Transfer q -> Doc AnsiStyle
ppTransfer ppQ (Transfer ins outs) = ppImage ppQ ins <+> ">->" <+> ppImage ppQ outs

ppRecipeKey :: RecipeKey -> Doc AnsiStyle
ppRecipeKey rk =
  ppMachine (view fMachine rk) <> annotate (color Black) "/" <> ppRecipeIdentifier (view fIdentifier rk)

ppRecipe :: Recipe -> Doc AnsiStyle
ppRecipe r =
  kw "recipe"
    <+> ppRecipeKey (view fKey r)
    <+> ppRecipeFunction r

ppRecipeFunction :: Recipe -> Doc AnsiStyle
ppRecipeFunction r =
  annotate
    (colorDull White)
    ( parens $
        (ppRealFloatMilli @Double . realToFrac . view fCycleTime $ r)
          <> annotate (color Black) "sec/cycle"
    )
    <> softline
    <> hang 2 (ppTransfer ppQuantity (view fTransfer r))

ppAnonymousBeltSt :: BeltSt -> Doc AnsiStyle
ppAnonymousBeltSt b =
  kw "belt"
    <+> "carrying"
    <+> ppItem (view fItem b)

ppNode :: Node -> Doc AnsiStyle
ppNode = annotate (color Yellow) . pretty

ppBeltKey :: (Node, Node) -> Doc AnsiStyle
ppBeltKey (np, ns) =
  (if np < 0 then kw ("external" <> show np) else ppNode np)
    <+> "↘"
    <+> (if ns < 0 then kw ("external" <> show ns) else ppNode ns)
      <> annotate (color Black) (pretty ':')

ppBeltSt :: (Node, Node) -> BeltSt -> Doc AnsiStyle
ppBeltSt k c = ppBeltKey k <+> ppAnonymousBeltSt c

ppBeltProp :: (Node, Node) -> (BeltSt, IsNew) -> Doc AnsiStyle
ppBeltProp k (b, New) = annotate (color Green) "new" <+> ppBeltSt k b
ppBeltProp k (b, _) = ppBeltSt k b

ppAnonymousBeltDy :: BeltDy -> Doc AnsiStyle
ppAnonymousBeltDy b =
  kw "belt"
    <+> "carrying"
    <+> ppItem (view fItem b)
    <+> parens
      ( ppPerMinute (view fEntering b)
          <+> ( case compare (view fEntering b) (view fExiting b) of
                  LT ->
                    ">-"
                      <+> annotate (colorDull Red) ("short" <+> ppPerMinute (shortfall b))
                      <+> "->"
                  EQ -> annotate (colorDull Green) ">->"
                  GT ->
                    ">-"
                      <+> annotate (colorDull Yellow) ("over" <+> ppPerMinute (overflow b))
                      <+> "->"
              )
          <+> ppPerMinute (view fExiting b)
      )

ppBeltDy :: (Node, Node) -> BeltDy -> Doc AnsiStyle
ppBeltDy k c = ppBeltKey k <+> ppAnonymousBeltDy c

ppAnonymousClusterSt :: ClusterSt -> Doc AnsiStyle
ppAnonymousClusterSt c =
  kw "cluster"
    <+> "of"
    <+> ppQuantity (view fQuantity c)
    <+> ppRecipeKey (view (fRecipe . fKey) c)
      <> ":"
    <+> ppRecipeFunction (view fRecipe c)

ppClusterSt :: Node -> ClusterSt -> Doc AnsiStyle
ppClusterSt n c = ppNode n <> annotate (color Black) (pretty ':') <+> ppAnonymousClusterSt c

ppClusterProp :: Node -> (ClusterSt, IsNew) -> Doc AnsiStyle
ppClusterProp n (c, New) = annotate (color Green) "new" <+> ppClusterSt n c
ppClusterProp n (c, _) = ppClusterSt n c

ppAnonymousClusterDy :: ClusterDy -> Doc AnsiStyle
ppAnonymousClusterDy c =
  kw "cluster"
    <+> "of"
    <+> ppQuantity (view fQuantity c)
    <+> ppRecipeKey (view (fRecipe . fKey) c)
      <> ":"
    <+> ppTransfer ppPerMinute (view fTransfer c)

ppClusterDy :: Node -> ClusterDy -> Doc AnsiStyle
ppClusterDy n c
  | view fMachine c == externalSource =
      kw ("external" <> show n)
        <+> "↓"
        <+> ppImage ppPerMinute (view (fTransfer . fOutputs) c)
  | view fMachine c == externalSink =
      kw ("byproducts" <> show n)
        <+> "↓"
        <+> ppImage ppPerMinute (view (fTransfer . fInputs) c)
  | otherwise =
      ppNode n <> annotate (color Black) (pretty ':') <+> ppAnonymousClusterDy c

coNodeLine :: String
coNodeLine = repeat '━'
coEdgeLine :: String
coEdgeLine = repeat '┄'
emptyLine :: String
emptyLine = repeat ' '

ppGraph
  :: forall a b bk g
   . (DynGraph g, Ord bk, Show bk)
  => (Node -> a -> Doc AnsiStyle)
  -> (b -> bk)
  -> ((Node, Node) -> b -> Doc AnsiStyle)
  -> g a b
  -> Doc AnsiStyle
ppGraph ppA bkf ppB g
  | [gc] <- comps = ppGraphComponent ppA bkf ppB gc
  | otherwise =
      vsep
        . fmap (ppNumberedGraphComponent ppA bkf ppB)
        . zip [1 ..]
        $ comps
 where
  comps = (`subgraph` g) <$> components g

ppNumberedGraphComponent
  :: forall a b bk g
   . (Graph g, Ord bk, Show bk)
  => (Node -> a -> Doc AnsiStyle)
  -> (b -> bk)
  -> ((Node, Node) -> b -> Doc AnsiStyle)
  -> (Int, g a b)
  -> Doc AnsiStyle
ppNumberedGraphComponent ppA bkf ppB (i, gc) =
  kw "component" <+> pretty i <> line <> indent 2 (ppGraphComponent ppA bkf ppB gc)

ppGraphComponent
  :: forall a b bk g
   . (Graph g, Ord bk, Show bk)
  => (Node -> a -> Doc AnsiStyle)
  -> (b -> bk)
  -> ((Node, Node) -> b -> Doc AnsiStyle)
  -> g a b
  -> Doc AnsiStyle
ppGraphComponent ppA bkf ppB g =
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
          . fmap (over _3 bkf)
          . filter (\(np, _, _) -> not (Set.member np validNodes))
          . labEdges
          $ g

  -- The final conversion from row data and dangling markers that never terminated into a Doc.
  render :: (Map (Node, Node, bk) Int, Seq (Text, Either (LNode a) (LEdge b))) -> Doc AnsiStyle
  render (danglingTrailingMarkers, rows) =
    vsep . concat $
      [ [ vsep
          ( errDoc "dangling predecessor edges:"
              : fmap
                (\(np, ns, bk) -> ppBeltKey (np, ns) <+> viaShow bk)
                (Map.keys danglingInitialMarkers)
          )
        | not (Map.null danglingTrailingMarkers)
        ]
      , map
          ( \(track, row) ->
              pretty track <+> case row of
                Left (n, a) -> ppA n a
                Right (np, ns, b) -> ppB (np, ns) b
          )
          (toList rows)
      , [ vsep
          ( errDoc "dangling successor edges:"
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
        terminating = Map.fromList $ pre <&> \(b, np) -> ((np, n, bkf b), 0 {- key ignored -})

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
                      , Map.insert (n, ns, bkf b) (b, c') m
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
            . renderColumns (Nothing, False, mempty)
            . sortOn snd
            . Map.toList
         where
          renderColumns
            :: (Maybe Int, Bool, TLB.Builder)
            -> [((Node, Node, bk), Int)]
            -> (Maybe Int, Bool, TLB.Builder)
          renderColumns s [] = s
          renderColumns (cMay, co, tb) (firstHit@(_, c') : rest) =
            let
              (hits, rest') = over _1 (firstHit :|) (span ((== c') . snd) rest)
              hit = hitf co (fst <$> hits)
              prefixLen = case cMay of
                Just c -> (c' - c) * 2 - 1
                Nothing -> c' * 2 + 1
            in
              renderColumns
                ( Just c'
                , co || isJust hit
                , tb
                    <> TLB.fromString (take prefixLen (if co then coLine else emptyLine))
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
ppFactorySt = ppGraph ppClusterSt (view fItem) ppBeltSt

ppFactoryProp :: FactoryProp -> Doc AnsiStyle
ppFactoryProp = ppGraph ppClusterProp (view (_1 . fItem)) ppBeltProp

ppFactoryDy :: FactoryDy -> Doc AnsiStyle
ppFactoryDy = ppGraph ppClusterDy (view fItem) ppBeltDy

ppVerifyError :: VerifyError -> Doc AnsiStyle
ppVerifyError = \case
  Verify.BeltUpNodeInvalid (np, ns, b) ->
    vsep
      [ errDoc $ "Error: Belt upstream (predecessor) node" <+> pretty np <+> "invalid."
      , indent 2 (ppBeltSt (np, ns) b)
      ]
  Verify.BeltDownNodeInvalid (np, ns, b) ->
    vsep
      [ errDoc $ "Error: Belt downstream (successor) node" <+> pretty ns <+> "invalid."
      , indent 2 (ppBeltSt (np, ns) b)
      ]
  Verify.UnknownItem i r ->
    vsep
      [ errDoc $ "Error: Unknown item " <> ppItem i
      , indent 2 (ppRecipe r)
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
    errDoc $ "Error: unknown recipe" <+> ppRecipeKey rk

ppLoadError :: LoadError -> Doc AnsiStyle
ppLoadError = \case
  Store.ParseError parseEx ->
    errDoc $ viaShow parseEx
  Store.InstantiateError instErrs ->
    vsep $ ppInstantiateError <$> Set.toList instErrs
  Store.VerifyError (errs, warns) ->
    vsep $ (ppVerifyWarning <$> Set.toList warns) <> (ppVerifyError <$> Set.toList errs)

ppLoadWarning :: LoadWarning -> Doc AnsiStyle
ppLoadWarning = \case
  Store.VerifyWarning warn ->
    ppVerifyWarning warn

ppProposal :: Int -> Proposal -> Maybe FactoryDy -> Doc AnsiStyle
ppProposal i p factDyMay =
  ( "Proposal"
      <+> annotate (color Yellow) (pretty i)
      <+> ( case view fResult p of
              Left _ -> "failed"
              Right () -> "succeeded"
          )
      <+> "after"
      <+> pretty (Seq.length (view fSteps p))
      <+> "steps"
      <+> parenthetical (ppProposalStep <$> toList (view fSteps p))
  )
    <> line
    <> indent
      2
      ( ( case view fResult p of
            Left perr -> errDoc (ppProposalError perr)
            Right () -> mempty
        )
          <> line
          <> indent 2 (vsep ["Structure:", indent 2 $ ppFactoryProp (view fFactory p)])
          <> line
          <> ( case factDyMay of
                Just factDy ->
                  indent 2 $ vsep ["Estimated:", indent 2 $ ppFactoryDy factDy]
                Nothing -> mempty
             )
      )

ppProposalError :: ProposalError -> Doc AnsiStyle
ppProposalError = \case
  Propose.NoRecipesProduceItem item ->
    errDoc ("No recipes found that produce" <+> ppItem item)
  Propose.ProposalStepsExceeded ->
    errDoc "maximum proposal steps exceeded"

ppProposalStep :: ProposalStep -> Doc AnsiStyle
ppProposalStep = \case
  Propose.TryRecipe recipe item rate pstepFor ->
    kw "add"
      <+> ppRecipeKey (view fKey recipe)
      <+> "to get"
      <+> ppPerMinute rate
      <+> ppItem item
      <+> ( case pstepFor of
              Propose.ProposalStepForGoal ->
                "for goal"
              Propose.ProposalStepForIntermediates ns ->
                "for intermediates" <+> bracketed (ppNode <$> ns)
          )

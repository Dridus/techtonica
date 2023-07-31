module TestUtils where

import Control.Lens (Wrapped (Unwrapped), andOf, each, view, _3, _Wrapped')
import Data.Fixed (Fixed, HasResolution)
import Data.Graph.Inductive (Graph, Node, labEdges, labNodes)
import Data.Map.Strict qualified as Map
import Data.Semialign.Indexed (ialignWith)
import Data.Text (unpack)
import Data.These (These (That, These, This))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Prettyprinter (Doc, align, defaultLayoutOptions, indent, layoutPretty, pretty, viaShow, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import Tech.Pretty qualified as P
import Tech.Types
import Test.QuickCheck (Property, conjoin, counterexample, (.&&.), (===))
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

neOp :: Doc AnsiStyle
neOp = P.kw "/="

assertFailureDoc :: HasCallStack => Doc AnsiStyle -> Assertion
assertFailureDoc = assertFailure . unpack . renderStrict . layoutPretty defaultLayoutOptions

assertBoolDoc :: HasCallStack => Doc AnsiStyle -> Bool -> Assertion
assertBoolDoc d = assertBool (unpack . renderStrict . layoutPretty defaultLayoutOptions $ d)

feqFixed :: HasResolution a => Fixed a -> Fixed a -> Fixed a -> Bool
feqFixed epsilon a b = abs (a - b) <= epsilon

peqFixed :: HasResolution a => Fixed a -> Fixed a -> Fixed a -> Property
peqFixed epsilon a b =
  counterexample (show a <> interpret res <> show b) res
 where
  res = feqFixed epsilon a b
  interpret True = " == "
  interpret False = " /= "

infix 4 `feqFixedDeci`
feqFixedDeci :: HasResolution a => Fixed a -> Fixed a -> Bool
feqFixedDeci = feqFixed (succ 0)

infix 4 `peqFixedDeci`
peqFixedDeci :: HasResolution a => Fixed a -> Fixed a -> Property
peqFixedDeci = peqFixed (succ 0)

infix 4 `feqWrappedFixedDeci`
feqWrappedFixedDeci :: (HasResolution a, Wrapped b, Unwrapped b ~ Fixed a) => b -> b -> Bool
feqWrappedFixedDeci = feqFixedDeci `on` view _Wrapped'

infix 4 `peqWrappedFixedDeci`
peqWrappedFixedDeci :: (HasResolution a, Wrapped b, Unwrapped b ~ Fixed a) => b -> b -> Property
peqWrappedFixedDeci = peqFixedDeci `on` view _Wrapped'

fcmpImageFixed
  :: forall q res
   . (HasResolution res, Show q, Wrapped q, Unwrapped q ~ Fixed res)
  => Image q
  -> Image q
  -> Image (String, Bool)
fcmpImageFixed = ialignWith comp
 where
  comp :: Item -> These q q -> (String, Bool)
  comp k (This aq) = (show k <> "(" <> show aq <> ") only present on left", False)
  comp k (That bq) = (show k <> "(" <> show bq <> ") only present on right", False)
  comp k (These aq bq) =
    let
      res = feqWrappedFixedDeci aq bq
      label = if res then " ≈ " else " ≉ "
    in
      ("at " <> show k <> " " <> show aq <> label <> show bq, res)

cmpImageRational
  :: forall q
   . (Show q, Wrapped q, Unwrapped q ~ Rational)
  => Image q
  -> Image q
  -> Image (String, Bool)
cmpImageRational = ialignWith comp
 where
  comp :: Item -> These q q -> (String, Bool)
  comp k (This aq) = (show k <> "(" <> show aq <> ") only present on left", False)
  comp k (That bq) = (show k <> "(" <> show bq <> ") only present on right", False)
  comp k (These aq bq) =
    let
      res = ((==) `on` view _Wrapped') aq bq
      label = if res then " ≈ " else " ≉ "
    in
      ("at " <> show k <> " " <> show aq <> label <> show bq, res)

infix 4 `feqImageFixed`
feqImageFixed
  :: forall q res
   . (HasResolution res, Show q, Wrapped q, Unwrapped q ~ Fixed res)
  => Image q
  -> Image q
  -> Bool
feqImageFixed = fmap (all snd . Map.elems) . fcmpImageFixed

infix 4 `eqImageRational`
eqImageRational
  :: forall q
   . (Show q, Wrapped q, Unwrapped q ~ Rational)
  => Image q
  -> Image q
  -> Bool
eqImageRational = fmap (all snd . Map.elems) . cmpImageRational

infix 4 `peqImageFixed`
peqImageFixed
  :: forall q res
   . (HasResolution res, Show q, Wrapped q, Unwrapped q ~ Fixed res)
  => Image q
  -> Image q
  -> Property
peqImageFixed = fmap (conjoin . fmap (uncurry counterexample) . Map.elems) . fcmpImageFixed

infix 4 `peqImageRational`
peqImageRational
  :: forall q
   . (Show q, Wrapped q, Unwrapped q ~ Rational)
  => Image q
  -> Image q
  -> Property
peqImageRational = fmap (conjoin . fmap (uncurry counterexample) . Map.elems) . cmpImageRational

infix 4 `feqTransferFixed`
feqTransferFixed
  :: forall q res
   . (HasResolution res, Show q, Wrapped q, Unwrapped q ~ Fixed res)
  => Transfer q
  -> Transfer q
  -> Bool
feqTransferFixed a b =
  (feqImageFixed `on` view fInputs) a b
    && (feqImageFixed `on` view fOutputs) a b

infix 4 `eqTransferRational`
eqTransferRational
  :: forall q
   . (Show q, Wrapped q, Unwrapped q ~ Rational)
  => Transfer q
  -> Transfer q
  -> Bool
eqTransferRational a b =
  (eqImageRational `on` view fInputs) a b
    && (eqImageRational `on` view fOutputs) a b

infix 4 `peqTransferFixed`
peqTransferFixed
  :: forall q res
   . (HasResolution res, Show q, Wrapped q, Unwrapped q ~ Fixed res)
  => Transfer q
  -> Transfer q
  -> Property
peqTransferFixed a b =
  (peqImageFixed `on` view fInputs) a b
    .&&. (peqImageFixed `on` view fOutputs) a b

infix 4 `peqTransferRational`
peqTransferRational
  :: forall q
   . (Show q, Wrapped q, Unwrapped q ~ Rational)
  => Transfer q
  -> Transfer q
  -> Property
peqTransferRational a b =
  (peqImageRational `on` view fInputs) a b
    .&&. (peqImageRational `on` view fOutputs) a b

infix 4 `feqRecipe`
feqRecipe :: Recipe -> Recipe -> Bool
feqRecipe a b =
  (feqFixedDeci `on` nominalDiffTimeToSeconds . view fCycleTime) a b
    && ((==) `on` view fKey) a b
    && (eqTransferRational `on` view fTransfer) a b

infix 4 `peqRecipe`
peqRecipe :: Recipe -> Recipe -> Property
peqRecipe a b =
  (peqFixedDeci `on` nominalDiffTimeToSeconds . view fCycleTime) a b
    .&&. ((===) `on` view fKey) a b
    .&&. (peqTransferRational `on` view fTransfer) a b

beltStShouldBe :: HasCallStack => BeltSt -> BeltSt -> Assertion
beltStShouldBe a b =
  unless (a == b) . assertFailureDoc . vsep $
    [ P.ppAnonymousBeltSt a <+> neOp <+> P.ppAnonymousBeltSt b
    , P.parens $ viaShow a <+> neOp <+> viaShow b
    ]

infix 4 `feqBeltDy`
feqBeltDy :: BeltDy -> BeltDy -> Bool
feqBeltDy a b =
  ((==) `on` view fItem) a b
    && ((==) `on` view fEntering) a b
    && ((==) `on` view fExiting) a b

beltDyShouldBe :: HasCallStack => BeltDy -> BeltDy -> Assertion
beltDyShouldBe a b =
  unless (a `feqBeltDy` b) . assertFailureDoc . vsep $
    [ P.ppAnonymousBeltDy a <+> neOp <+> P.ppAnonymousBeltDy b
    , P.parens $ viaShow a <+> neOp <+> viaShow b
    ]

infix 4 `feqClusterSt`
feqClusterSt :: ClusterSt -> ClusterSt -> Bool
feqClusterSt a b =
  (feqRecipe `on` view fRecipe) a b
    && ((==) `on` view fQuantity) a b

clusterStShouldBe :: HasCallStack => ClusterSt -> ClusterSt -> Assertion
clusterStShouldBe a b =
  unless (a `feqClusterSt` b) . assertFailureDoc . vsep $
    [ P.ppAnonymousClusterSt a <+> neOp <+> P.ppAnonymousClusterSt b
    , P.parens $ viaShow a <+> neOp <+> viaShow b
    ]

infix 4 `feqClusterDy`
feqClusterDy :: ClusterDy -> ClusterDy -> Bool
feqClusterDy a b =
  (feqRecipe `on` view fRecipe) a b
    && ((==) `on` view fQuantity) a b
    && (eqTransferRational `on` view fTransfer) a b

clusterDyShouldBe :: HasCallStack => ClusterDy -> ClusterDy -> Assertion
clusterDyShouldBe a b =
  unless (a `feqClusterDy` b) . assertFailureDoc . vsep $
    [ P.ppAnonymousClusterDy a <+> neOp <+> P.ppAnonymousClusterDy b
    , P.parens $ viaShow a <+> neOp <+> viaShow b
    ]

ppGraph :: (Graph g, Show a, Show b) => (a -> Doc ann) -> (b -> Doc ann) -> g a b -> Doc ann
ppGraph ppA ppB g =
  vsep $
    (ppLNode ppA <$> labNodes g)
      <> (ppLEdge ppB <$> labEdges g)
      <> (ppLNode viaShow <$> labNodes g)
      <> (ppLEdge viaShow <$> labEdges g)
 where
  ppLNode pp (n, a) =
    "node"
      <+> pretty n <> ":"
      <+> align (pp a)
  ppLEdge pp (np, ns, b) =
    "edge" <+> pretty np <> " -> " <> pretty ns <> ":" <+> align (pp b)

fcmpGraph
  :: (Graph g, Show a, Show bk, Ord bk, Show b)
  => (a -> a -> Bool)
  -> (a -> Doc ann)
  -> (b -> bk)
  -> (b -> b -> Bool)
  -> (b -> Doc ann)
  -> g a b
  -> g a b
  -> (Map Node (String, String, Bool), Map (Node, Node, bk) (String, String, Bool))
fcmpGraph eqA ppA bkf eqB ppB ga gb = (nodes, edges)
 where
  nodes = ialignWith (fcmp "node" eqA ppA) (Map.fromList $ labNodes ga) (Map.fromList $ labNodes gb)
  edges = ialignWith (fcmp "edge" eqB ppB) (mapEdges ga) (mapEdges gb)
  mapEdges = Map.fromList . fmap (\(np, ns, b) -> ((np, ns, bkf b), b)) . labEdges
  ppMay pp (Just a) = align (pp a)
  ppMay _ Nothing = "<missing>"
  fcmp which eq pp k these =
    let (lhsMay, rhsMay, res, disposition :: Text) = case these of
          This lhs -> (Just lhs, Nothing, False, "missing in right")
          That rhs -> (Nothing, Just rhs, False, "missing in left")
          These lhs rhs -> let r = eq lhs rhs in (Just lhs, Just rhs, r, if r then "matches" else "doesn't match")
    in  ( show . vsep $
            [ which <+> viaShow k
            , indent 2 . vsep $
                [ pretty disposition
                , "left:" <+> ppMay pp lhsMay
                , "right:" <+> ppMay pp rhsMay
                ]
            ]
        , show . vsep $
            [ which <+> viaShow k
            , indent 2 . vsep $
                [ "detail"
                , "left:" <+> ppMay viaShow lhsMay
                , "right:" <+> ppMay viaShow rhsMay
                ]
            ]
        , res
        )

graphShouldBe
  :: (HasCallStack, Graph g, Show a, Show bk, Ord bk, Show b)
  => (a -> a -> Bool)
  -> (a -> Doc ann)
  -> (b -> bk)
  -> (b -> b -> Bool)
  -> (b -> Doc ann)
  -> g a b
  -> g a b
  -> Assertion
graphShouldBe eqA ppA bkf eqB ppB ga gb =
  unless (andOf (each . _3) nodeCmps && andOf (each . _3) edgeCmps) . assertFailure $
    "actual graph does not match expected graph:"
      <> foldMap
        ( \(msg, _, res) ->
            bool ("\n" <> msg) [] res
        )
        (Map.elems nodeCmps <> Map.elems edgeCmps)
      <> foldMap
        ( \(_, msg, res) ->
            bool ("\n" <> msg) [] res
        )
        (Map.elems nodeCmps <> Map.elems edgeCmps)
 where
  (nodeCmps, edgeCmps) = fcmpGraph eqA ppA bkf eqB ppB ga gb

peqGraph
  :: (Graph g, Show a, Show bk, Ord bk, Show b)
  => (a -> a -> Bool)
  -> (a -> Doc ann)
  -> (b -> bk)
  -> (b -> b -> Bool)
  -> (b -> Doc ann)
  -> g a b
  -> g a b
  -> Property
peqGraph eqA ppA bkf eqB ppB ga gb =
  conjoin . fmap render $ Map.elems nodeCmps <> Map.elems edgeCmps
 where
  (nodeCmps, edgeCmps) = fcmpGraph eqA ppA bkf eqB ppB ga gb
  render (ppMsg, showMsg, res) = counterexample (ppMsg <> "\n" <> showMsg) res

factoryStShouldBe :: HasCallStack => FactorySt -> FactorySt -> Assertion
factoryStShouldBe =
  graphShouldBe (==) P.ppAnonymousClusterSt (view fItem) (==) P.ppAnonymousBeltSt

factoryDyShouldBe :: HasCallStack => FactoryDy -> FactoryDy -> Assertion
factoryDyShouldBe =
  graphShouldBe feqClusterDy P.ppAnonymousClusterDy (view fItem) feqBeltDy P.ppAnonymousBeltDy

module Tech.Mermaid where

import Control.Lens (view, _Wrapped')
import Data.Graph.Inductive (DynGraph, Graph, Node, components, labEdges, labNodes, subgraph)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLBI
import Data.Text.Lazy.Builder.RealFloat qualified as TLBRF
import Tech.Planner.Estimate (externalSinkId, externalSourceId)
import Tech.Types

renderItem :: Item -> TLB.Builder
renderItem = TLB.fromText . view _Wrapped'

renderRecipeKey :: RecipeKey -> TLB.Builder
renderRecipeKey rk =
  (TLB.fromText . view (fMachineIdentifier . _Wrapped') $ rk)
    <> "/"
    <> (TLB.fromText . view (fIdentifier . _Wrapped') $ rk)

renderQuantity :: Quantity -> TLB.Builder
renderQuantity = TLBRF.formatRealFloat TLBRF.Fixed (Just 3) . realToFrac @Rational @Double . view _Wrapped'

renderPerMinute :: PerMinute -> TLB.Builder
renderPerMinute r =
  (TLBRF.formatRealFloat TLBRF.Fixed (Just 3) . realToFrac @Rational @Double . unPerMinute $ r)
    <> "/min"

renderImage :: (q -> TLB.Builder) -> Image q -> TLB.Builder
renderImage renderQ im =
  case pairs of
    [] -> "()"
    [p] -> renderPair p
    _ -> foldMap renderPair pairs
 where
  pairs = Map.toList im
  renderPair (i, q) = renderQ q <> " " <> renderItem i <> "<br>"

renderTransfer :: (q -> TLB.Builder) -> Transfer q -> TLB.Builder
renderTransfer renderQ txfr =
  (renderImage renderQ . view fInputs $ txfr)
    <> "#8659;<br>" -- ⇓
    <> (renderImage renderQ . view fOutputs $ txfr)

renderAnonymousClusterSt :: ClusterSt -> TLB.Builder
renderAnonymousClusterSt c =
  (renderQuantity . view fQuantity $ c)
    <> "#215; " -- ×
    <> (renderRecipeKey . view (fRecipe . fKey) $ c)

renderClusterSt :: Node -> ClusterSt -> TLB.Builder
renderClusterSt n c = "[#" <> TLBI.decimal n <> "] " <> renderAnonymousClusterSt c

renderAnonymousClusterDy :: ClusterDy -> TLB.Builder
renderAnonymousClusterDy c =
  (renderQuantity . view fQuantity $ c)
    <> "#215; " -- ×
    <> (renderRecipeKey . view (fRecipe . fKey) $ c)
    <> "<br>"
    <> "#8212;<br>" -- —
    <> (renderTransfer renderPerMinute . view fTransfer $ c)

renderClusterDy :: Node -> ClusterDy -> TLB.Builder
renderClusterDy n c
  | view (fMachine . fIdentifier) c == externalSourceId = "external source " <> TLBI.decimal n
  | view (fMachine . fIdentifier) c == externalSinkId = "byproducts " <> TLBI.decimal n
  | otherwise = "[#" <> TLBI.decimal n <> "] " <> renderAnonymousClusterDy c

renderAnonymousBeltSt :: BeltSt -> TLB.Builder
renderAnonymousBeltSt = renderItem . view fItem

renderBeltSt :: (Node, Node) -> BeltSt -> TLB.Builder
renderBeltSt _ = renderAnonymousBeltSt

renderAnonymousBeltDy :: BeltDy -> TLB.Builder
renderAnonymousBeltDy b =
  (renderItem . view fItem $ b)
    <> "<br>"
    <> (renderPerMinute . view fEntering $ b)
    <> " "
    <> ( case compare (view fEntering b) (view fExiting b) of
          LT ->
            "#10522; " -- >-
              <> "short "
              <> renderPerMinute (shortfall b)
              <> " #8594;" -- ->
          EQ -> "#8611;" -- >->
          GT ->
            "#10522; " -- >-
              <> "over "
              <> renderPerMinute (overflow b)
              <> " #8594;" -- ->
       )
    <> " "
    <> renderPerMinute (view fExiting b)

renderBeltDy :: (Node, Node) -> BeltDy -> TLB.Builder
renderBeltDy _ = renderAnonymousBeltDy

renderGraph
  :: DynGraph g
  => (Node -> a -> TLB.Builder)
  -> ((Node, Node) -> b -> TLB.Builder)
  -> g a b
  -> TLB.Builder
renderGraph renderA renderB g
  | [gc] <- comps = header <> renderGraphComponent renderA renderB gc
  | otherwise =
      (header <>)
        . foldMap (renderNumberedGraphComponent renderA renderB)
        . zip [1 ..]
        $ comps
 where
  comps = (`subgraph` g) <$> components g
  header = "graph TD\n"

renderNumberedGraphComponent
  :: Graph g
  => (Node -> a -> TLB.Builder)
  -> ((Node, Node) -> b -> TLB.Builder)
  -> (Int, g a b)
  -> TLB.Builder
renderNumberedGraphComponent renderA renderB (i, gc) =
  "subgraph sg"
    <> TLBI.decimal i
    <> " [component "
    <> TLBI.decimal i
    <> "]\n"
    <> renderGraphComponent renderA renderB gc
    <> "end\n"

renderGraphComponent
  :: Graph g
  => (Node -> a -> TLB.Builder)
  -> ((Node, Node) -> b -> TLB.Builder)
  -> g a b
  -> TLB.Builder
renderGraphComponent renderA renderB gc =
  foldMap renderLNode (labNodes gc)
    <> foldMap renderLEdge (labEdges gc)
 where
  nid n = "n" <> bool (TLBI.decimal n) ("e" <> TLBI.decimal (negate n)) (n < 0)
  renderLNode la@(n, _) = "  " <> nid n <> "(\"" <> uncurry renderA la <> "\")\n"
  renderLEdge (np, ns, b) = "  " <> nid np <> "-->|\"" <> renderB (np, ns) b <> "\"|" <> nid ns <> "\n"

graphFactorySt :: FactorySt -> LText
graphFactorySt = TLB.toLazyText . renderGraph renderClusterSt renderBeltSt

graphFactoryDy :: FactoryDy -> LText
graphFactoryDy = TLB.toLazyText . renderGraph renderClusterDy renderBeltDy

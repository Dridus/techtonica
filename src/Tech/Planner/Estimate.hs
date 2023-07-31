module Tech.Planner.Estimate where

import Control.Lens (
  concatMapOf,
  each,
  filtered,
  foldrOf,
  ifolded,
  ifoldrOf,
  ix,
  over,
  preview,
  sumOf,
  toListOf,
  view,
 )
import Data.Align (align)
import Data.Graph.Inductive (Context, Gr, LEdge, LNode, Node, components, labEdges, labNodes, mkGraph, subgraph)
import Data.Map.Strict qualified as Map
import Data.These (These (That, These, This), fromThese)
import Tech.Graph (contexts, edges, nodeLabel, nodes, preAdjs, sucAdjs)
import Tech.Machines (externalSink, externalSource)
import Tech.Types

externalRecipe :: Machine -> Recipe
externalRecipe m = Recipe (RecipeKey m "<external>") 0.0 (Transfer mempty mempty)

externalClusterSt :: Machine -> ClusterSt
externalClusterSt m =
  ClusterSt
    { _clusterSt_recipe = externalRecipe m
    , _clusterSt_quantity = 0.0
    }

externalClusterDy :: Machine -> Transfer Rate -> ClusterDy
externalClusterDy m transfer =
  ClusterDy
    { _clusterDy_static = externalClusterSt m
    , _clusterDy_transfer = transfer
    }

estimate :: FactorySt -> FactoryDy
estimate =
  reassembleComponents
    . fmap (\(nep, gin) -> estimateComponent nep (pred nep) gin)
    . zip [-1, -3 ..]
    . splitComponents

reassembleComponents :: [Gr a b] -> Gr a b
reassembleComponents gins = mkGraph (concatMap labNodes gins) (concatMap labEdges gins)

splitComponents :: Gr a b -> [Gr a b]
splitComponents gin = (`subgraph` gin) <$> components gin

estimateComponent :: Node -> Node -> FactorySt -> FactoryDy
estimateComponent nsource nsync =
  -- FIXME calculate efficiency and propagate efficiency loss down
  -- FIXME belt capacity
  assignFlows nsource nsync
    . assignClusterRates

assignClusterRates :: Gr ClusterSt b -> Gr ClusterDy b
assignClusterRates = over (nodes . each) $ \su ->
  let
    _clusterDy_static = su
    _clusterDy_transfer = recipeRate (view fQuantity su) (view fRecipe su)
  in
    ClusterDy {..}

recipeRate :: Quantity -> Recipe -> Transfer Rate
recipeRate q r =
  fmap (Rate . runIdentity)
    . flip divT (realToFrac . view fCycleTime $ r)
    . mulT (unQuantity q)
    . fmap (Identity . unQuantity)
    . view fTransfer
    $ r

assignFlows :: Node -> Node -> Gr ClusterDy BeltSt -> Gr ClusterDy BeltDy
assignFlows nsource nsync gin =
  mkGraph
    (makeExternalSource flows nsource : makeExternalSinks flows nsync : labNodes gin)
    flows
 where
  flows :: [LEdge BeltDy]
  flows =
    concatMapOf each finalizeFlow . Map.toList $ align outflows inflows
   where
    outflows = outflowsOf gin nsync
    inflows = inflowsOf gin nsource outflows
    finalizeFlow
      :: ((Node, Node), These (Map Item Rate) (Map Item Rate))
      -> [(Node, Node, BeltDy)]
    finalizeFlow ((np, ns), fromThese mempty mempty -> (upOutflows, downInflows)) =
      Map.toList (align upOutflows downInflows)
        <&> \(i, fromThese 0.0 0.0 -> (upOutflow, downInflow)) ->
          let
            beltEntering
              | np == nsource = downInflow
              | otherwise = upOutflow
            beltExiting
              | ns == nsync = upOutflow
              | otherwise = downInflow
          in
            (np, ns, BeltDy (BeltSt i) beltEntering beltExiting)

makeExternalSource :: [LEdge BeltDy] -> Node -> LNode ClusterDy
makeExternalSource flows n = (n, externalClusterDy externalSource (Transfer mempty image))
 where
  image :: Image Rate
  image = Map.fromListWith (+) $ concatMapOf each f flows
  f (np, _, BeltDy (BeltSt i) _ outf)
    | np == n = [(i, outf)]
    | otherwise = []

makeExternalSinks :: [LEdge BeltDy] -> Node -> LNode ClusterDy
makeExternalSinks flows n = (n, externalClusterDy externalSink (Transfer image mempty))
 where
  image :: Image Rate
  image = Map.fromListWith (+) $ concatMapOf each f flows
  f (_, ns, BeltDy (BeltSt i) inf _)
    | ns == n = [(i, inf)]
    | otherwise = []

inflowsOf :: Gr ClusterDy BeltSt -> Node -> Map (Node, Node) (Image Rate) -> Map (Node, Node) (Image Rate)
inflowsOf gin nsource outflows =
  Map.unionsWith addI
    . map (inflowsOfContext nsource outflows)
    . toListOf contexts
    $ gin

inflowsOfContext :: Node -> Map (Node, Node) (Image Rate) -> Context ClusterDy BeltSt -> Map (Node, Node) (Image Rate)
inflowsOfContext nsource outflows (pre, nDown, clDown, _) =
  Map.unionsWith addI . fmap f . Map.toList $ align supplies demands
 where
  supplies :: Image (Map Node Rate)
  supplies =
    Map.unionsWith (Map.unionWith (+)) $
      pre <&> \(BeltSt i, nUp) ->
        case preview (ix (nUp, nDown) . ix i) outflows of
          Just outf -> Map.singleton i (Map.singleton nUp outf)
          Nothing -> mempty

  demands :: Image Rate
  demands = view (fTransfer . fInputs) clDown

  f :: (Item, These (Map Node Rate) Rate) -> Map (Node, Node) (Image Rate)
  f (_, This _) = mempty -- let the belt overflow
  f (i, That need) = Map.singleton (nsource, nDown) (Map.singleton i need)
  f (i, These offer need) =
    let
      supply = sumOf each offer
      sat = max 1.0 (supply / need)
    in
      Map.fromList $ Map.toList offer <&> \(nUp, r) -> ((nUp, nDown), Map.singleton i (r * sat))

outflowsOf :: Gr ClusterDy BeltSt -> Node -> Map (Node, Node) (Image Rate)
outflowsOf gin nsync =
  Map.unionsWith addI . map (outflowsOfContext nsync) . toListOf contexts $ gin

outflowsOfContext :: Node -> Context ClusterDy BeltSt -> Map (Node, Node) (Image Rate)
outflowsOfContext nsync (_, nUp, clUp, suc) =
  Map.unionsWith addI . fmap f . Map.toList $ align supply consumers
 where
  consumers :: Image [Node]
  consumers = Map.fromListWith (<>) $ suc <&> \(BeltSt i, n) -> (i, [n])

  supply :: Image Rate
  supply = view (fTransfer . fOutputs) clUp

  g :: [Node] -> Image Rate -> Map (Node, Node) (Image Rate)
  g nDowns im = Map.fromList $ nDowns <&> \nDown -> ((nUp, nDown), im)

  f :: (Item, These Rate [Node]) -> Map (Node, Node) (Image Rate)
  f (i, This excess) = g [nsync] (Map.singleton i excess)
  f (i, That nDowns) = g nDowns (Map.singleton i 0.0)
  f (i, These inf nDowns) = g nDowns (Map.singleton i (inf / fromIntegral (length nDowns)))

overflowsOf :: FactoryDy -> Image (Map Node Rate)
overflowsOf = ifoldrOf (edges . ifolded) f mempty
 where
  f (np, _) bdy m =
    case overflow bdy of
      r
        | r > 0 ->
            let i = view fItem bdy
            in  Map.alter (Just . maybe (Map.singleton np r) (Map.insertWith (+) np r)) i m
      _ -> m

feedstocksOf :: FactoryDy -> Image (Map Node Rate)
feedstocksOf =
  foldrOf
    (contexts . filtered ((== externalSource) . view (nodeLabel . fMachine)) . sucAdjs . each)
    f
    mempty
 where
  f (bdy, ns) m =
    let
      i = view fItem bdy
      r = view fExiting bdy
    in
      Map.alter (Just . maybe (Map.singleton ns r) (Map.insertWith (+) ns r)) i m

byproductsOf :: FactoryDy -> Image (Map Node Rate)
byproductsOf =
  foldrOf
    (contexts . filtered ((== externalSink) . view (nodeLabel . fMachine)) . preAdjs . each)
    f
    mempty
 where
  f (bdy, np) m =
    let
      i = view fItem bdy
      r = view fEntering bdy
    in
      Map.alter (Just . maybe (Map.singleton np r) (Map.insertWith (+) np r)) i m

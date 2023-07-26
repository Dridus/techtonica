module Tech.Planner where

import Control.Lens (concatMapOf, each, ix, preview, sumOf, to, toListOf, view, _1, _Just, _Wrapped')
import Data.Align (align)
import Data.Graph.Inductive (Context, Gr, LEdge, LNode, Node, labNodes, match, mkGraph, nmap)
import Data.Map.Strict qualified as Map
import Data.These (These (That, These, This), fromThese)
import Tech.Machines (external)
import Tech.Types

estimate :: FactorySt -> FactoryDy
estimate =
  -- FIXME calculate efficiency and propagate efficiency loss down
  -- FIXME belt capacity
  assignFlows
    . assignClusterRates

nExternalSource, nExternalSink :: Node
nExternalSource = -1
nExternalSink = -2

externalRecipe :: Recipe
externalRecipe = Recipe (RecipeKey external "<external>") 0.0 (Transfer mempty mempty)

assignFlows :: Gr ClusterDy BeltSt -> Gr ClusterDy BeltDy
assignFlows gin =
  mkGraph
    (externalSource : externalSink : labNodes gin)
    flows
 where
  externalSource :: LNode ClusterDy
  externalSource = (nExternalSource, ClusterDy externalRecipe 0.0 (Transfer mempty externalSourceImage))
  externalSourceImage :: Image Rate
  externalSourceImage = Map.fromListWith (+) $ concatMapOf each f flows
   where
    f (nUp, _, BeltDy i _ outf)
      | nUp == nExternalSource = [(i, outf)]
      | otherwise = []

  externalSink :: LNode ClusterDy
  externalSink = (nExternalSink, ClusterDy externalRecipe 0.0 (Transfer externalSinkImage mempty))
  externalSinkImage :: Image Rate
  externalSinkImage = Map.fromListWith (+) $ concatMapOf each f flows
   where
    f (_, nDown, BeltDy i inf _)
      | nDown == nExternalSink = [(i, inf)]
      | otherwise = []

  flows :: [LEdge BeltDy]
  flows =
    concatMapOf each finalizeFlow . Map.toList $ align outflows inflows
   where
    outflows = outflowsOf gin
    inflows = inflowsOf gin outflows
    finalizeFlow
      :: ((Node, Node), These (Map Item Rate) (Map Item Rate))
      -> [(Node, Node, BeltDy)]
    finalizeFlow ((np, ns), fromThese mempty mempty -> (upOutflows, downInflows)) =
      Map.toList (align upOutflows downInflows)
        <&> \(i, fromThese 0.0 0.0 -> (upOutflow, downInflow)) ->
          let
            beltEntering
              | np == nExternalSource = downInflow
              | otherwise = upOutflow
            beltExiting
              | ns == nExternalSink = upOutflow
              | otherwise = downInflow
          in
            (np, ns, BeltDy i beltEntering beltExiting)

inflowsOf :: Gr ClusterDy BeltSt -> Map (Node, Node) (Image Rate) -> Map (Node, Node) (Image Rate)
inflowsOf gin outflows =
  Map.unionsWith addI
    . map (inflowsOfContext outflows)
    . toListOf (each . _1 . to (`match` gin) . _1 . _Just)
    . labNodes
    $ gin

inflowsOfContext :: Map (Node, Node) (Image Rate) -> Context ClusterDy BeltSt -> Map (Node, Node) (Image Rate)
inflowsOfContext outflows (pre, nDown, clDown, _) =
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
  demands = view (transfer . inputs) clDown

  f :: (Item, These (Map Node Rate) Rate) -> Map (Node, Node) (Image Rate)
  f (_, This _) = mempty -- let the belt overflow
  f (i, That need) = Map.singleton (nExternalSource, nDown) (Map.singleton i need)
  f (i, These offer need) =
    let
      supply = sumOf each offer
      sat = max 1.0 (supply / need)
    in
      Map.fromList $ Map.toList offer <&> \(nUp, r) -> ((nUp, nDown), Map.singleton i (r * sat))

outflowsOf :: Gr ClusterDy BeltSt -> Map (Node, Node) (Image Rate)
outflowsOf gin =
  Map.unionsWith addI
    . map outflowsOfContext
    . toListOf (each . _1 . to (`match` gin) . _1 . _Just)
    . labNodes
    $ gin

outflowsOfContext :: Context ClusterDy BeltSt -> Map (Node, Node) (Image Rate)
outflowsOfContext (_, nUp, clUp, suc) =
  Map.unionsWith addI . fmap f . Map.toList $ align supply consumers
 where
  consumers :: Image [Node]
  consumers = Map.fromListWith (<>) $ suc <&> \(BeltSt i, n) -> (i, [n])

  supply :: Image Rate
  supply = view (transfer . outputs) clUp

  g :: [Node] -> Image Rate -> Map (Node, Node) (Image Rate)
  g nDowns im = Map.fromList $ nDowns <&> \nDown -> ((nUp, nDown), im)

  f :: (Item, These Rate [Node]) -> Map (Node, Node) (Image Rate)
  f (i, This excess) = g [nExternalSink] (Map.singleton i excess)
  f (i, That nDowns) = g nDowns (Map.singleton i 0.0)
  f (i, These inf nDowns) = g nDowns (Map.singleton i (inf / fromIntegral (length nDowns)))

assignClusterRates :: Gr ClusterSt b -> Gr ClusterDy b
assignClusterRates = nmap $ \su ->
  let
    _clusterDy_recipe = view recipe su
    _clusterDy_quantity = view quantity su
    _clusterDy_transfer =
      fmap (Rate . runIdentity)
        . flip divT (view (recipe . cycleTime . to realToFrac) su)
        . mulT (view (quantity . _Wrapped') su)
        . fmap (Identity . unQuantity)
        $ view (recipe . transfer) su
  in
    ClusterDy {..}

{-# LANGUAGE UndecidableInstances #-}

module Tech.Planner.Propose where

import Control.Lens (
  Fold,
  Lens',
  asIndex,
  assign,
  at,
  each,
  filtered,
  ifolded,
  ix,
  makeLensesWith,
  modifying,
  over,
  preview,
  set,
  sumOf,
  toListOf,
  use,
  view,
  withIndex,
  _1,
  _2,
  _Wrapped',
 )
import Control.Monad.RWS.Strict (RWS, runRWS)
import Data.Align (alignWith)
import Data.Graph.Inductive (Gr, Node)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.These (These (..))
import Tech.Graph (allAdjLabels, disjunctContexts, edges, nodeLabel, nodes)
import Tech.LensOptions (techFields)
import Tech.Planner.Estimate (byproductsOf, estimate, feedstocksOf, overflowsOf, recipeRate)
import Tech.Recipes (Recipes, filterRecipes, producing)
import Tech.Types

-- * Errors

data ProposalConstraints f = ProposalConstraints
  { _proposalConstraints_assumeGiven :: Set Item
  , _proposalConstraints_useOverflows :: f Bool
  }

-- * Constraints

deriving stock instance Show (f Bool) => Show (ProposalConstraints f)
makeLensesWith techFields ''ProposalConstraints

instance Semigroup (ProposalConstraints Last) where
  a <> b =
    ProposalConstraints
      { _proposalConstraints_assumeGiven = merge fAssumeGiven
      , _proposalConstraints_useOverflows = merge fUseOverflows
      }
   where
    merge :: Semigroup a => Lens' (ProposalConstraints Last) a -> a
    merge op = (flip (<>) `on` view op) a b
instance Monoid (ProposalConstraints Last) where
  mempty = ProposalConstraints mempty (Last Nothing)

assumeGiven :: Foldable f => f Item -> ProposalConstraints Last
assumeGiven = flip (set fAssumeGiven . Set.fromList . toList) mempty

doUseOverflows :: ProposalConstraints Last
doUseOverflows = set fUseOverflows (Last $ Just True) mempty

dontUseOverflows :: ProposalConstraints Last
dontUseOverflows = set fUseOverflows (Last $ Just False) mempty

fixConstraints :: ProposalConstraints Last -> ProposalConstraints Identity
fixConstraints pc =
  ProposalConstraints
    { _proposalConstraints_assumeGiven = view fAssumeGiven pc
    , _proposalConstraints_useOverflows =
        Identity
          . fromMaybe False
          . getLast
          . view fUseOverflows
          $ pc
    }

-- * Proposal

-- ** Results

data ProposalError
  = NoRecipesProduceItem
      {_planningError_item :: Item}
  | ProposalStepsExceeded
deriving stock instance Eq ProposalError
deriving stock instance Ord ProposalError
deriving stock instance Show ProposalError

type ProposalResult = Either ProposalError ()

data ProposalStepFor = ProposalStepForGoal | ProposalStepForIntermediates [Node]
deriving stock instance Eq ProposalStepFor
deriving stock instance Ord ProposalStepFor
deriving stock instance Show ProposalStepFor

data ProposalStep = TryRecipe
  { _proposalStep_recipe :: Recipe
  , _proposalStep_item :: Item
  , _proposalStep_rate :: PerMinute
  , _proposalStep_for :: ProposalStepFor
  }
deriving stock instance Eq ProposalStep
deriving stock instance Ord ProposalStep
deriving stock instance Show ProposalStep
makeLensesWith techFields ''ProposalStep

data IsNew = New | Existing
deriving stock instance Eq IsNew
deriving stock instance Ord IsNew
deriving stock instance Show IsNew

type FactoryProp = Gr (ClusterSt, IsNew) (BeltSt, IsNew)

factoryStFromProp :: FactoryProp -> FactorySt
factoryStFromProp =
  over
    (disjunctContexts . each)
    (over nodeLabel fst . over allAdjLabels fst)

data Proposal = Proposal
  { _proposal_factory :: FactoryProp
  , _proposal_steps :: Seq ProposalStep
  , _proposal_result :: Either ProposalError ()
  }
deriving stock instance Eq Proposal
deriving stock instance Show Proposal
makeLensesWith techFields ''Proposal

type Proposals = Seq Proposal

-- ** Internal State

data ProposeEnv = ProposeEnv
  { _proposeEnv_recipes :: Recipes
  , _proposeEnv_original :: FactorySt
  , _proposeEnv_goal :: Image PerMinute
  , _proposeEnv_constraints :: ProposalConstraints Identity
  }
deriving stock instance Show ProposeEnv
makeLensesWith techFields ''ProposeEnv

data ProposalEnv = ProposalEnv
  { _proposalEnv_parentEnv :: ProposeEnv
  , _proposalEnv_estimate :: FactoryDy
  , _proposalEnv_overflows :: Image (Map Node PerMinute)
  , _proposalEnv_byproducts :: Image (Map Node PerMinute)
  }
deriving stock instance Show ProposalEnv
makeLensesWith techFields ''ProposalEnv
instance Has_fRecipes ProposalEnv Recipes where fRecipes = fParentEnv . fRecipes
instance Has_fOriginal ProposalEnv FactorySt where fOriginal = fParentEnv . fOriginal
instance Has_fGoal ProposalEnv (Image PerMinute) where fGoal = fParentEnv . fGoal
instance Has_fConstraints ProposalEnv (ProposalConstraints Identity) where
  fConstraints = fParentEnv . fConstraints

data UnfinishedProposal = UnfinishedProposal
  { _unfinishedProposal_factory :: FactoryProp
  , _unfinishedProposal_steps :: Seq ProposalStep
  }
deriving stock instance Show UnfinishedProposal
makeLensesWith techFields ''UnfinishedProposal

data ProposeState = ProposeState
  { _proposeState_unfinished :: [UnfinishedProposal]
  , _proposeState_finished :: Proposals
  }
deriving stock instance Show ProposeState
makeLensesWith techFields ''ProposeState

-- ** Algorithm

maxProposalSteps :: Int
maxProposalSteps = 100

propose :: Recipes -> FactorySt -> Image PerMinute -> ProposalConstraints Last -> Proposals
propose recipes gin needs (fixConstraints -> constraints) =
  view _1 $ runRWS go env (ProposeState [state0] mempty)
 where
  env =
    ProposeEnv
      { _proposeEnv_recipes = recipes
      , _proposeEnv_original = gin
      , _proposeEnv_goal = needs
      , _proposeEnv_constraints = constraints
      }
  state0 =
    UnfinishedProposal
      { _unfinishedProposal_factory =
          over
            (disjunctContexts . each)
            (over nodeLabel (,Existing) . over allAdjLabels (,Existing))
            gin
      , _unfinishedProposal_steps = mempty
      }
  go :: RWS ProposeEnv () ProposeState Proposals
  go =
    use fUnfinished >>= \case
      [] -> use fFinished
      (p : ps) -> do
        assign fUnfinished ps
        traverse_ (modifying fFinished . flip (:|>)) =<< elaborateProposal p
        go

failUnfinishedProposal :: UnfinishedProposal -> ProposalError -> Proposal
failUnfinishedProposal up err =
  Proposal
    { _proposal_factory = view fFactory up
    , _proposal_steps = view fSteps up
    , _proposal_result = Left err
    }

elaborateProposal :: UnfinishedProposal -> RWS ProposeEnv () ProposeState (Maybe Proposal)
elaborateProposal up
  | Seq.length (view fSteps up) > maxProposalSteps =
      pure . Just $ failUnfinishedProposal up ProposalStepsExceeded
  | otherwise = do
      let factDy = estimate . factoryStFromProp . view fFactory $ up

      -- always review whether the goal state is satisfied even if we've previously satisfied it,
      -- in case satisfying an intermediate requirement has consumed those byproducts
      --
      -- run both tactics and if both have nothing to do take that as success

      env <- ask
      let localEnv =
            ProposalEnv
              { _proposalEnv_parentEnv = env
              , _proposalEnv_estimate = factDy
              , _proposalEnv_overflows = overflowsOf factDy
              , _proposalEnv_byproducts = byproductsOf factDy
              }
      let tactics = trySatisfyGoal up <|> trySatisfyIntermediates up
      (res, s', ()) <- runRWS (runMaybeT tactics) localEnv <$> get
      put s'
      case res of
        Nothing ->
          pure . Just $
            Proposal
              { _proposal_factory = view fFactory up
              , _proposal_steps = view fSteps up
              , _proposal_result = Right ()
              }
        Just r ->
          pure r

type ProposalTacticM = MaybeT (RWS ProposalEnv () ProposeState)

trySatisfyGoal :: UnfinishedProposal -> ProposalTacticM (Maybe Proposal)
trySatisfyGoal up = do
  overflowSummary <- over each (sumOf each) <$> view fOverflows
  goal <- view fGoal
  assumedGiven <- view (fConstraints . fAssumeGiven)
  useOverflows <- view (fConstraints . fUseOverflows . _Wrapped')
  byproducts <- view fByproducts
  let need =
        goal
          & subtractSupply (if useOverflows then overflowSummary else mempty)
          & subtractSupply (over each (sumOf each) byproducts)
          & (`Map.withoutKeys` assumedGiven)
          & Map.filter (> 0)

  case Map.minViewWithKey need of
    Nothing -> mzero
    Just ((item, rate), _) -> do
      nextAddCapacity up ProposalStepForGoal item rate []

trySatisfyIntermediates :: UnfinishedProposal -> ProposalTacticM (Maybe Proposal)
trySatisfyIntermediates up = do
  -- look for new clusters which have edges to the external source, indicating unsatified needs
  -- we should try to fulfill

  factDy <- view fEstimate
  assumedGiven <- view $ fConstraints . fAssumeGiven
  let newClusters =
        Set.fromList $
          toListOf
            (nodes . ifolded . filtered ((== New) . view _2) . asIndex)
            (view fFactory up)
  let unsatisfied =
        Map.filter (not . Map.null)
          . fmap (`Map.restrictKeys` newClusters)
          . (`Map.withoutKeys` assumedGiven)
          . feedstocksOf
          $ factDy

  -- now pick an item arbitrarily that we don't assume is given and is needed by one of our
  -- newly added clusters. if none, zero out indicating there are no unsatisified needs that
  -- we've been asked to address
  (item, downstreams) <- maybe mzero pure $ preview (ifolded . withIndex) unsatisfied
  let beltSt = (BeltSt {_beltSt_item = item}, New)
  let downstreamNodes = toListOf (ifolded . asIndex) downstreams
  let need0 = sumOf each downstreams

  -- first try to add in belts to satisfy (some of) the need by using existing factory
  -- byproducts or overflows (if allowed)
  --
  -- the ordering is deterministic but otherwise unplanned, so instead of allocating smartly
  -- belts are created as a dense network

  (factProp', need') <- flip execStateT (view fFactory up, need0) $ do
    let
      addBeltsFrom
        :: Fold ProposalEnv (Image (Map Node PerMinute))
        -> StateT (FactoryProp, PerMinute) ProposalTacticM ()
      addBeltsFrom source = do
        satisfied <- sumOf (source . ix item . each) <$> ask
        modifying _2 $ max 0 . subtract satisfied
        upstreams <- toListOf (source . ix item . ifolded . asIndex) <$> ask
        for_ ((,) <$> upstreams <*> downstreamNodes) $ \(np, ns) ->
          modifying _1 $ over (edges . ix (np, ns)) (beltSt :)

    whenM (view (fConstraints . fUseOverflows . _Wrapped')) $
      addBeltsFrom fOverflows

    addBeltsFrom fByproducts

  -- and then finally add capacity if the need is unsatisfied after tying in those
  nextAddCapacity
    (up {_unfinishedProposal_factory = factProp'})
    (ProposalStepForIntermediates downstreamNodes)
    item
    need'
    downstreamNodes

nextAddCapacity
  :: (MonadReader r m, Has_fRecipes r Recipes, MonadState ProposeState m)
  => UnfinishedProposal
  -> ProposalStepFor
  -> Item
  -> PerMinute
  -> [Node]
  -> m (Maybe Proposal)
nextAddCapacity up pstepFor item rate downstreamNodes = do
  -- this naively picks the first item (by lexical order, incidentally) and tries to solve
  -- it, not considering optimality in any way. it should be alright as it's relatively rare
  -- to have competing multi-output recipes to choose amongst, but maybe could use improvement
  --
  -- regardless of the proposal traversal order, it might be nice for proposals to be
  -- sorted by some optimality, e.g. total external resource cost
  recipes <- view fRecipes
  let candidateRecipes = filterRecipes (producing item) recipes
  if null candidateRecipes
    then pure . Just $ failUnfinishedProposal up (NoRecipesProduceItem item)
    else do
      traverse_
        (void . tryRecipe up pstepFor item rate downstreamNodes)
        candidateRecipes
      pure Nothing

nextUnfinishedProposal :: MonadState ProposeState m => UnfinishedProposal -> ProposalStep -> FactoryProp -> m ()
nextUnfinishedProposal progenitor step factProp = modifying fUnfinished (offspring :)
 where
  offspring =
    UnfinishedProposal
      { _unfinishedProposal_factory = factProp
      , _unfinishedProposal_steps = view fSteps progenitor :|> step
      }

subtractSupply :: (Num q, Ord q) => Image q -> Image q -> Image q
subtractSupply = alignWith $ \case
  This _ -> 0
  That need -> need
  These given need -> max 0 (need - given)

tryRecipe
  :: MonadState ProposeState m
  => UnfinishedProposal
  -> ProposalStepFor
  -> Item
  -> PerMinute
  -> [Node]
  -> Recipe
  -> m Node
tryRecipe up pstepFor item rate downstreamNodes recipe =
  node <$ nextUnfinishedProposal up step factProp
 where
  step =
    TryRecipe
      { _proposalStep_recipe = recipe
      , _proposalStep_item = item
      , _proposalStep_rate = rate
      , _proposalStep_for = pstepFor
      }
  productionRate =
    fromMaybe
      (error "tryRecipe: recipe does not produce requested item")
      . preview (fOutputs . ix item)
      $ recipeRate 1.0 recipe
  clust =
    ClusterSt
      { _clusterSt_recipe = recipe
      , _clusterSt_quantity = fromIntegral @Integer . ceiling $ rate / productionRate
      }
  node = newNode (view fFactory up)
  beltSt = (BeltSt {_beltSt_item = item}, New)
  factProp =
    foldl'
      (\g ns -> over (edges . ix (node, ns)) (beltSt :) g)
      (set (nodes . at node) (Just (clust, New)) $ view fFactory up)
      downstreamNodes

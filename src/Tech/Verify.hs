module Tech.Verify where

import Control.Lens (asIndex, both, has, ifolded, ix, modifying, to, toListOf, view, _1, _2, _3)
import Control.Monad.Except (throwError)
import Control.Monad.RWS.Strict (RWS, runRWS)
import Data.Graph.Inductive (Context, LEdge, LNode, Node, labEdges, match)
import Data.Set qualified as Set
import Tech.Types

data VerifyError
  = BeltUpNodeInvalid
      {_verifyError_edge :: LEdge BeltSt}
  | BeltDownNodeInvalid
      {_verifyError_edge :: LEdge BeltSt}
  | UnknownItem
      { _verifyError_item :: Item
      , _verifyError_recipe :: Recipe
      }
deriving stock instance Eq VerifyError
deriving stock instance Ord VerifyError
deriving stock instance Show VerifyError

data VerifyWarning
  = BeltItemNotOutputByUp
      { _verifyWarning_upNode :: LNode ClusterSt
      , _verifyWarning_edge :: LEdge BeltSt
      }
  | BeltItemNotInputForDown
      { _verifyWarning_downNode :: LNode ClusterSt
      , _verifyWarning_edge :: LEdge BeltSt
      }
deriving stock instance Eq VerifyWarning
deriving stock instance Ord VerifyWarning
deriving stock instance Show VerifyWarning

verifyFactorySt :: FactorySt -> (Set VerifyWarning, Either (Set VerifyError) ())
verifyFactorySt gin
  | Set.null errors = (warnings, Right ())
  | otherwise = (warnings, Left errors)
 where
  ((), (warnings, errors), ()) = runRWS verifyAll gin mempty

type VerifyM = RWS FactorySt () (Set VerifyWarning, Set VerifyError)
type VerifyExceptM = ExceptT VerifyError VerifyM

class MonadVerify m where liftVerify :: VerifyM a -> m a
instance MonadVerify VerifyM where liftVerify = id
instance MonadVerify VerifyExceptM where liftVerify = lift

tryVerify :: VerifyExceptM a -> VerifyM (Maybe a)
tryVerify =
  runExceptT >=> \case
    Left err -> Nothing <$ raiseVerifyError err
    Right a -> pure $ Just a

tryVerify_ :: VerifyExceptM () -> VerifyM ()
tryVerify_ = void . tryVerify

raiseVerifyWarning :: MonadVerify m => VerifyWarning -> m ()
raiseVerifyWarning = liftVerify . modifying _1 . Set.insert

raiseVerifyError :: MonadVerify m => VerifyError -> m ()
raiseVerifyError = liftVerify . modifying _2 . Set.insert

wantContext :: MonadVerify m => Node -> m (Maybe (Context ClusterSt BeltSt))
wantContext n = liftVerify . asks $ fst . match n

needContext :: Node -> VerifyError -> VerifyExceptM (Context ClusterSt BeltSt)
needContext n e = wantContext n >>= maybe (throwError e) pure

needCluster :: Node -> VerifyError -> VerifyExceptM ClusterSt
needCluster = (fmap . fmap) (view _3) . needContext

verifyAll :: VerifyM ()
verifyAll = do
  asks labEdges >>= traverse_ verifyOneBeltItems

verifyOneBeltItems :: LEdge BeltSt -> VerifyM ()
verifyOneBeltItems e@(np, ns, b) = do
  tryVerify_ $ do
    cl <- needCluster np (BeltUpNodeInvalid e)
    unless (has (fRecipe . fTransfer . fOutputs . ix (view fItem b)) cl) $
      raiseVerifyWarning (BeltItemNotOutputByUp (np, cl) e)
  tryVerify_ $ do
    cl <- needCluster ns (BeltDownNodeInvalid e)
    unless (has (fRecipe . fTransfer . fInputs . ix (view fItem b)) cl) $
      raiseVerifyWarning (BeltItemNotInputForDown (ns, cl) e)

verifyRecipe :: Set Item -> Recipe -> (Set VerifyWarning, Either (Set VerifyError) ())
verifyRecipe knownItems r
  | Set.null unregisteredItems = (mempty, Right ())
  | otherwise = (mempty, Left $ Set.mapMonotonic (`UnknownItem` r) unregisteredItems)
 where
  usedItems =
    Set.fromList
      . toListOf (fTransfer . to (view fInputs &&& view fOutputs) . both . ifolded . asIndex)
      $ r
  unregisteredItems = Set.difference usedItems knownItems

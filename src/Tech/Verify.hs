module Tech.Verify where

import Control.Lens (has, ix, modifying, view, _1, _2, _3)
import Control.Lens.TH (makeLensesWith, underscoreFields)
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
makeLensesWith underscoreFields ''VerifyError
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
makeLensesWith underscoreFields ''VerifyWarning
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
    unless (has (recipe . transfer . outputs . ix (view item b)) cl) $
      raiseVerifyWarning (BeltItemNotOutputByUp (np, cl) e)
  tryVerify_ $ do
    cl <- needCluster ns (BeltDownNodeInvalid e)
    unless (has (recipe . transfer . inputs . ix (view item b)) cl) $
      raiseVerifyWarning (BeltItemNotInputForDown (ns, cl) e)

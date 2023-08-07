module Tech.Store where

import Control.Lens (each, ix, over, preview, toListOf, view, _Left)
import Control.Lens.TH (makeLensesWith)
import Control.Monad.RWS.Strict (RWS, runRWS)
import Data.Aeson.TH (deriveJSON)
import Data.Graph.Inductive (LEdge, LNode, Node, labEdges, labNodes, mkGraph)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Yaml (decodeEither', encode)
import Data.Yaml qualified as Yaml
import Tech.LensOptions (techFields)
import Tech.Recipes (indexRecipes, recipeKeyOptic)
import Tech.Store.AesonOptions (aesonOptions)
import Tech.Store.Orphans ()
import Tech.Types
import Tech.Verify (VerifyError, VerifyWarning, verifyFactorySt)

data ClusterSpec = ClusterSpec
  { _clusterSpec_node :: Node
  , _clusterSpec_recipeKey :: RecipeKey
  , _clusterSpec_quantity :: Quantity
  }
deriving stock instance Show ClusterSpec
deriveJSON aesonOptions ''ClusterSpec
makeLensesWith techFields ''ClusterSpec

data BeltSpec = BeltSpec
  { _beltSpec_upstream :: Node
  , _beltSpec_downstream :: Node
  , _beltSpec_item :: Item
  }
deriving stock instance Show BeltSpec
deriveJSON aesonOptions ''BeltSpec
makeLensesWith techFields ''BeltSpec

data FactorySpec = FactorySpec
  { _factorySpec_clusters :: [ClusterSpec]
  , _factorySpec_belts :: [BeltSpec]
  }
deriving stock instance Show FactorySpec
deriveJSON aesonOptions ''FactorySpec
makeLensesWith techFields ''FactorySpec

data FactoryEnvSpec = FactoryEnvSpec
  { _factoryEnvSpec_items :: Set Item
  , _factoryEnvSpec_machines :: [Machine]
  , _factoryEnvSpec_recipes :: [Recipe]
  }
deriving stock instance Show FactoryEnvSpec
deriveJSON aesonOptions ''FactoryEnvSpec
makeLensesWith techFields ''FactoryEnvSpec

data InstantiateError
  = UnrecognizedRecipeIdentifier RecipeKey
  | UnrecognizedMachineIdentifierForRecipe RecipeKey
deriving stock instance Eq InstantiateError
deriving stock instance Ord InstantiateError
deriving stock instance Show InstantiateError

data LoadError
  = ParseError Yaml.ParseException
  | InstantiateError (Set InstantiateError)
  | VerifyError (Set VerifyError, Set VerifyWarning)
deriving stock instance Show LoadError

newtype LoadWarning
  = VerifyWarning VerifyWarning
deriving stock instance Show LoadWarning

clusterSpecFromSt :: LNode ClusterSt -> ClusterSpec
clusterSpecFromSt (n, c) =
  ClusterSpec
    { _clusterSpec_node = n
    , _clusterSpec_recipeKey = view (fRecipe . fKey) c
    , _clusterSpec_quantity = view fQuantity c
    }

beltSpecFromSt :: LEdge BeltSt -> BeltSpec
beltSpecFromSt (np, ns, b) =
  BeltSpec
    { _beltSpec_upstream = np
    , _beltSpec_downstream = ns
    , _beltSpec_item = view fItem b
    }

factorySpecFromSt :: FactorySt -> FactorySpec
factorySpecFromSt factSt =
  FactorySpec
    { _factorySpec_clusters = clusterSpecFromSt <$> labNodes factSt
    , _factorySpec_belts = beltSpecFromSt <$> labEdges factSt
    }

instantiateBeltSpec :: BeltSpec -> LEdge BeltSt
instantiateBeltSpec bs = (view fUpstream bs, view fDownstream bs, BeltSt (view fItem bs))

needRecipe :: RecipeKey -> MaybeT (RWS FactoryEnv () (Set InstantiateError)) Recipe
needRecipe rk =
  maybe
    ( modify (Set.insert (UnrecognizedRecipeIdentifier rk)) >> mzero
    )
    pure
    =<< preview (fRecipes . recipeKeyOptic rk)

needMachine :: RecipeKey -> MaybeT (RWS FactoryEnv () (Set InstantiateError)) Machine
needMachine rk =
  maybe
    ( modify (Set.insert (UnrecognizedMachineIdentifierForRecipe rk)) >> mzero
    )
    pure
    =<< preview (fMachines . ix (view fMachineIdentifier rk))

instantiateClusterSpec
  :: ClusterSpec
  -> RWS FactoryEnv () (Set InstantiateError) (Maybe (LNode ClusterSt))
instantiateClusterSpec clusterSpec = runMaybeT $ do
  let rk = view fRecipeKey clusterSpec
  r <- needRecipe rk
  m <- needMachine rk
  pure
    ( view fNode clusterSpec
    , ClusterSt
        { _clusterSt_recipe = r
        , _clusterSt_machine = m
        , _clusterSt_quantity = view fQuantity clusterSpec
        }
    )

instantiateFactorySpec
  :: MonadReader FactoryEnv m
  => FactorySpec
  -> m (Either (Set InstantiateError) FactorySt)
instantiateFactorySpec factSpec =
  ask <&> \env ->
    case runRWS go env mempty of
      (factSt, errs, ()) | Set.null errs -> Right factSt
      (_, errs, ()) -> Left errs
 where
  go = do
    nodes <- catMaybes <$> traverse instantiateClusterSpec (view fClusters factSpec)
    let edges = instantiateBeltSpec <$> view fBelts factSpec
    pure $ mkGraph nodes edges

storeFactorySpec :: FactorySpec -> ByteString
storeFactorySpec = encode

loadFactorySpec :: ByteString -> Either Yaml.ParseException FactorySpec
loadFactorySpec = decodeEither'

loadFactory
  :: MonadReader FactoryEnv m
  => ByteString
  -> m (Either LoadError ([LoadWarning], FactorySt))
loadFactory bs = runExceptT $ do
  factSpec <- ExceptT $ pure . over _Left ParseError . loadFactorySpec $ bs
  factSt <- ExceptT $ over _Left InstantiateError <$> instantiateFactorySpec factSpec
  ExceptT . pure $ case verifyFactorySt factSt of
    (verifyWarns, Left verifyErrs) -> Left (VerifyError (verifyErrs, verifyWarns))
    (verifyWarns, Right ()) ->
      pure
        ( VerifyWarning <$> Set.toList verifyWarns
        , factSt
        )

loadFactoryFile
  :: (MonadReader FactoryEnv m, MonadIO m)
  => FilePath
  -> m (Either LoadError ([LoadWarning], FactorySt))
loadFactoryFile = loadFactory <=< readFileBS

storeFactory :: FactorySt -> ByteString
storeFactory = storeFactorySpec . factorySpecFromSt

storeFactoryFile :: MonadIO m => FilePath -> FactorySt -> m ()
storeFactoryFile fp = writeFileBS fp . storeFactory

envSpecFromEnv :: FactoryEnv -> FactoryEnvSpec
envSpecFromEnv env =
  FactoryEnvSpec
    { _factoryEnvSpec_items = view fItems env
    , _factoryEnvSpec_machines = toListOf (fMachines . each) env
    , _factoryEnvSpec_recipes = toListOf (fRecipes . each . each) env
    }

loadEnvSpec :: ByteString -> Either Yaml.ParseException FactoryEnvSpec
loadEnvSpec = decodeEither'

storeEnvSpec :: FactoryEnvSpec -> ByteString
storeEnvSpec = encode

loadEnv :: ByteString -> Either LoadError ([LoadWarning], FactoryEnv)
loadEnv bs = do
  envSpec <- over _Left ParseError $ loadEnvSpec bs
  let ms = Map.fromList . fmap (view fIdentifier &&& id) . view fMachines $ envSpec
  let rs = indexRecipes . view fRecipes $ envSpec
  let fenv =
        FactoryEnv
          { _factoryEnv_items = view fItems envSpec
          , _factoryEnv_machines = ms
          , _factoryEnv_recipes = rs
          }
  pure (mempty, fenv)

loadEnvFile :: FilePath -> IO (Either LoadError ([LoadWarning], FactoryEnv))
loadEnvFile = fmap loadEnv . readFileBS

storeEnv :: FactoryEnv -> ByteString
storeEnv = storeEnvSpec . envSpecFromEnv

storeEnvFile :: FilePath -> FactoryEnv -> IO ()
storeEnvFile fp = writeFileBS fp . storeEnv

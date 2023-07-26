module Tech.Store where

import Control.Lens (each, toListOf, view, _2, _Left, over)
import Control.Lens.TH (makeLensesWith, underscoreFields)
import Control.Lens.Unsound (lensProduct)
import Data.Aeson.TH (deriveJSON)
import Data.Graph.Inductive (LEdge, LNode, Node, labEdges, labNodes, mkGraph)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Yaml (decodeEither', decodeFileEither, encode)
import Data.Yaml qualified as Yaml
import Tech.Recipes (builtinRecipes, findRecipeByKey)
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
makeLensesWith underscoreFields ''ClusterSpec

data BeltSpec = BeltSpec
  { _beltSpec_upstream :: Node
  , _beltSpec_downstream :: Node
  , _beltSpec_item :: Item
  }
deriving stock instance Show BeltSpec
deriveJSON aesonOptions ''BeltSpec
makeLensesWith underscoreFields ''BeltSpec

data FactorySpec = FactorySpec
  { _factorySpec_clusters :: [ClusterSpec]
  , _factorySpec_belts :: [BeltSpec]
  , _factorySpec_customRecipes :: [Recipe]
  }
deriving stock instance Show FactorySpec
deriveJSON aesonOptions ''FactorySpec
makeLensesWith underscoreFields ''FactorySpec

data InstantiateError
  = UnrecognizedRecipeIdentifier RecipeKey
deriving stock instance Eq InstantiateError
deriving stock instance Ord InstantiateError
deriving stock instance Show InstantiateError

data LoadError
  = ParseError Yaml.ParseException
  | InstantiateError (Set InstantiateError)
  | VerifyError (Set VerifyError, Set VerifyWarning)
deriving stock instance Show LoadError

data LoadWarning
  = VerifyWarning VerifyWarning
deriving stock instance Show LoadWarning

clusterSpecFromSt :: LNode ClusterSt -> ClusterSpec
clusterSpecFromSt (n, c) =
  ClusterSpec
    { _clusterSpec_node = n
    , _clusterSpec_recipeKey = view (recipe . key) c
    , _clusterSpec_quantity = view quantity c
    }

beltSpecFromSt :: LEdge BeltSt -> BeltSpec
beltSpecFromSt (np, ns, b) =
  BeltSpec
    { _beltSpec_upstream = np
    , _beltSpec_downstream = ns
    , _beltSpec_item = view item b
    }

factorySpecFromSt :: FactorySt -> FactorySpec
factorySpecFromSt factSt =
  FactorySpec
    { _factorySpec_clusters = clusterSpecFromSt <$> labNodes factSt
    , _factorySpec_belts = beltSpecFromSt <$> labEdges factSt
    , _factorySpec_customRecipes
    }
 where
  usedRecipes :: Map RecipeKey Recipe
  usedRecipes =
    Map.fromList
      . toListOf (each . _2 . recipe . (key `lensProduct` id))
      . labNodes
      $ factSt
  _factorySpec_customRecipes =
    Map.elems . Map.filterWithKey (const . isNothing . findRecipeByKey builtinRecipes) $ usedRecipes

instantiateBeltSpec :: BeltSpec -> LEdge BeltSt
instantiateBeltSpec bs = (view upstream bs, view downstream bs, BeltSt (view item bs))

instantiateClusterSpec
  :: Map Machine (Map RecipeIdentifier Recipe)
  -> ClusterSpec
  -> State (Set InstantiateError) (Maybe (LNode ClusterSt))
instantiateClusterSpec recipes clusterSpec =
  let rk = view recipeKey clusterSpec
  in  case findRecipeByKey recipes rk of
        Nothing -> Nothing <$ modify (Set.insert (UnrecognizedRecipeIdentifier rk))
        Just r ->
          pure . Just $
            ( view node clusterSpec
            , ClusterSt
                { _clusterSt_recipe = r
                , _clusterSt_quantity = view quantity clusterSpec
                }
            )

instantiateCustomRecipes
  :: Map Machine (Map RecipeIdentifier Recipe)
  -> Recipe
  -> Map Machine (Map RecipeIdentifier Recipe)
instantiateCustomRecipes rest r =
  Map.insertWith (<>) (view (key . machine) r) (Map.singleton (view (key . identifier) r) r) rest

instantiateFactorySpec :: FactorySpec -> Either (Set InstantiateError) FactorySt
instantiateFactorySpec factSpec =
  case runState go mempty of
    (factSt, errs) | Set.null errs -> Right factSt
    (_, errs) -> Left errs
 where
  go = do
    let recipes = foldl' instantiateCustomRecipes builtinRecipes (view customRecipes factSpec)
    nodes <- catMaybes <$> traverse (instantiateClusterSpec recipes) (view clusters factSpec)
    let edges = instantiateBeltSpec <$> view belts factSpec
    pure $ mkGraph nodes edges

storeFactorySpec :: FactorySpec -> ByteString
storeFactorySpec = encode

storeFactorySpecFile :: FilePath -> FactorySpec -> IO ()
storeFactorySpecFile fp = writeFileBS fp . storeFactorySpec

loadFactorySpec :: ByteString -> Either Yaml.ParseException FactorySpec
loadFactorySpec = decodeEither'

loadFactorySpecFile :: FilePath -> IO (Either Yaml.ParseException FactorySpec)
loadFactorySpecFile = decodeFileEither

loadFactory :: ByteString -> Either LoadError ([LoadWarning], FactorySt)
loadFactory bs = do
  factSpec <- over _Left ParseError $ loadFactorySpec bs
  factSt <- over _Left InstantiateError $ instantiateFactorySpec factSpec
  case verifyFactorySt factSt of
    (verifyWarns, Left verifyErrs) -> Left (VerifyError (verifyErrs, verifyWarns))
    (verifyWarns, Right ()) ->
      pure
        ( VerifyWarning <$> Set.toList verifyWarns
        , factSt
        )

loadFactoryFile :: FilePath -> IO (Either LoadError ([LoadWarning], FactorySt))
loadFactoryFile = fmap loadFactory . readFileBS

storeFactory :: FactorySt -> ByteString
storeFactory = storeFactorySpec . factorySpecFromSt

storeFactoryFile :: FilePath -> FactorySt -> IO ()
storeFactoryFile fp = writeFileBS fp . storeFactory

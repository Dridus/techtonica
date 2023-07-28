module Tech.Store where

import Control.Lens (each, over, toListOf, view, _2, _Left)
import Control.Lens.TH (makeLensesWith, underscoreFields)
import Control.Lens.Unsound (lensProduct)
import Data.Aeson.TH (deriveJSON)
import Data.Graph.Inductive (LEdge, LNode, Node, labEdges, labNodes, mkGraph)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Yaml (decodeEither', encode)
import Data.Yaml qualified as Yaml
import Tech.Recipes (Recipes, findRecipeByKey, indexRecipes, unindexRecipes)
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

data RecipesSpec = RecipesSpec
  { _recipesSpec_items :: Set Item
  , _recipesSpec_recipes :: [Recipe]
  }
deriving stock instance Show RecipesSpec
deriveJSON aesonOptions ''RecipesSpec
makeLensesWith underscoreFields ''RecipesSpec

newtype InstantiateError
  = UnrecognizedRecipeIdentifier RecipeKey
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

factorySpecFromSt :: Recipes -> FactorySt -> FactorySpec
factorySpecFromSt knownRecipes factSt =
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
    Map.elems . Map.filterWithKey (const . isNothing . findRecipeByKey knownRecipes) $ usedRecipes

instantiateBeltSpec :: BeltSpec -> LEdge BeltSt
instantiateBeltSpec bs = (view upstream bs, view downstream bs, BeltSt (view item bs))

instantiateClusterSpec
  :: Map Machine (Map RecipeIdentifier Recipe)
  -> ClusterSpec
  -> State (Set InstantiateError) (Maybe (LNode ClusterSt))
instantiateClusterSpec knownRecipes clusterSpec =
  let rk = view recipeKey clusterSpec
  in  case findRecipeByKey knownRecipes rk of
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

instantiateFactorySpec :: Recipes -> FactorySpec -> Either (Set InstantiateError) FactorySt
instantiateFactorySpec knownRecipes factSpec =
  case runState go mempty of
    (factSt, errs) | Set.null errs -> Right factSt
    (_, errs) -> Left errs
 where
  go = do
    let allRecipes = foldl' instantiateCustomRecipes knownRecipes (view customRecipes factSpec)
    nodes <- catMaybes <$> traverse (instantiateClusterSpec allRecipes) (view clusters factSpec)
    let edges = instantiateBeltSpec <$> view belts factSpec
    pure $ mkGraph nodes edges

storeFactorySpec :: FactorySpec -> ByteString
storeFactorySpec = encode

loadFactorySpec :: ByteString -> Either Yaml.ParseException FactorySpec
loadFactorySpec = decodeEither'

loadFactory :: Recipes -> ByteString -> Either LoadError ([LoadWarning], FactorySt)
loadFactory knownRecipes bs = do
  factSpec <- over _Left ParseError $ loadFactorySpec bs
  factSt <- over _Left InstantiateError $ instantiateFactorySpec knownRecipes factSpec
  case verifyFactorySt factSt of
    (verifyWarns, Left verifyErrs) -> Left (VerifyError (verifyErrs, verifyWarns))
    (verifyWarns, Right ()) ->
      pure
        ( VerifyWarning <$> Set.toList verifyWarns
        , factSt
        )

loadFactoryFile :: Recipes -> FilePath -> IO (Either LoadError ([LoadWarning], FactorySt))
loadFactoryFile knownRecipes = fmap (loadFactory knownRecipes) . readFileBS

storeFactory :: Recipes -> FactorySt -> ByteString
storeFactory knownRecipes = storeFactorySpec . factorySpecFromSt knownRecipes

storeFactoryFile :: Recipes -> FilePath -> FactorySt -> IO ()
storeFactoryFile knownRecipes fp = writeFileBS fp . storeFactory knownRecipes

recipesSpecFromRecipes :: (Set Item, Recipes) -> RecipesSpec
recipesSpecFromRecipes (its, m) = RecipesSpec its (unindexRecipes m)

instantiateRecipesFromSpec :: RecipesSpec -> Recipes
instantiateRecipesFromSpec = indexRecipes . view recipes

loadRecipesSpec :: ByteString -> Either Yaml.ParseException RecipesSpec
loadRecipesSpec = decodeEither'

storeRecipesSpec :: RecipesSpec -> ByteString
storeRecipesSpec = encode

loadRecipes :: ByteString -> Either LoadError ([LoadWarning], (Set Item, Recipes))
loadRecipes bs = do
  recipesSpec <- over _Left ParseError $ loadRecipesSpec bs
  let rs = instantiateRecipesFromSpec recipesSpec
  pure (mempty, (view items recipesSpec, rs))

loadRecipesFile :: FilePath -> IO (Either LoadError ([LoadWarning], (Set Item, Recipes)))
loadRecipesFile = fmap loadRecipes . readFileBS

storeRecipes :: (Set Item, Recipes) -> ByteString
storeRecipes = storeRecipesSpec . recipesSpecFromRecipes

storeRecipesFile :: FilePath -> (Set Item, Recipes) -> IO ()
storeRecipesFile fp = writeFileBS fp . storeRecipes

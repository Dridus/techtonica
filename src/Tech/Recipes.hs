module Tech.Recipes where

import Control.Lens (each, filtered, has, ix, preview, to, toListOf, view)
import Data.Map.Strict qualified as Map
import Tech.Types

type Recipes = Map Machine (Map RecipeIdentifier Recipe)

findRecipeByKey :: Recipes -> RecipeKey -> Maybe Recipe
findRecipeByKey recipes (RecipeKey m rid) = preview (ix m . ix rid) recipes

insertRecipe :: Recipe -> Recipes -> Recipes
insertRecipe r =
  Map.insertWith (<>) (view (key . machine) r) (Map.singleton (view (key . identifier) r) r)

indexRecipes :: [Recipe] -> Recipes
indexRecipes =
  Map.unionsWith (<>)
    . toListOf
      ( each
          . to
            ( Map.singleton
                <$> view (key . machine)
                <*> (Map.singleton <$> view (key . identifier) <*> id)
            )
      )

unindexRecipes :: Recipes -> [Recipe]
unindexRecipes = toListOf (each . each)

filterRecipes :: (Recipe -> Bool) -> Recipes -> [Recipe]
filterRecipes p = toListOf (each . each . filtered p)

producing :: Item -> Recipe -> Bool
producing i = has $ transfer . outputs . ix i

consuming :: Item -> Recipe -> Bool
consuming i = has $ transfer . inputs . ix i

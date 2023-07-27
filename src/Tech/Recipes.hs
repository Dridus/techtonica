module Tech.Recipes where

import Control.Lens (each, ix, preview, to, toListOf, view)
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

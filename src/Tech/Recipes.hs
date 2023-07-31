module Tech.Recipes where

import Control.Lens (each, filtered, has, ix, preview, to, toListOf, view)
import Data.Map.Strict qualified as Map
import Tech.Types

type Recipes = Map Machine (Map RecipeIdentifier Recipe)

findRecipeByKey :: Recipes -> RecipeKey -> Maybe Recipe
findRecipeByKey recipes (RecipeKey m rid) = preview (ix m . ix rid) recipes

insertRecipe :: Recipe -> Recipes -> Recipes
insertRecipe r =
  Map.insertWith (<>) (view (fKey . fMachine) r) (Map.singleton (view (fKey . fIdentifier) r) r)

indexRecipes :: [Recipe] -> Recipes
indexRecipes =
  Map.unionsWith (<>)
    . toListOf
      ( each
          . to
            ( Map.singleton
                <$> view (fKey . fMachine)
                <*> (Map.singleton <$> view (fKey . fIdentifier) <*> id)
            )
      )

unindexRecipes :: Recipes -> [Recipe]
unindexRecipes = toListOf (each . each)

filterRecipes :: (Recipe -> Bool) -> Recipes -> [Recipe]
filterRecipes p = toListOf (each . each . filtered p)

producing :: Item -> Recipe -> Bool
producing i = has $ fTransfer . fOutputs . ix i

consuming :: Item -> Recipe -> Bool
consuming i = has $ fTransfer . fInputs . ix i

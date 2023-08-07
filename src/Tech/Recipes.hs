module Tech.Recipes where

import Control.Lens (each, filtered, has, ix, preview, to, toListOf, view, Traversal')
import Data.Map.Strict qualified as Map
import Tech.Types

recipeKeyOptic :: RecipeKey -> Traversal' Recipes Recipe
recipeKeyOptic (RecipeKey m rid) = ix m . ix rid

findRecipeByKey :: Recipes -> RecipeKey -> Maybe Recipe
findRecipeByKey recipes rk = preview (recipeKeyOptic rk) recipes

insertRecipe :: Recipe -> Recipes -> Recipes
insertRecipe r =
  Map.insertWith (<>) (view (fKey . fMachineIdentifier) r) (Map.singleton (view (fKey . fIdentifier) r) r)

indexRecipes :: [Recipe] -> Recipes
indexRecipes =
  Map.unionsWith (<>)
    . toListOf
      ( each
          . to
            ( Map.singleton
                <$> view (fKey . fMachineIdentifier)
                <*> (Map.singleton <$> view (fKey . fIdentifier) <*> id)
            )
      )

filterRecipes :: (Recipe -> Bool) -> Recipes -> [Recipe]
filterRecipes p = toListOf (each . each . filtered p)

producing :: Item -> Recipe -> Bool
producing i = has $ fTransfer . fOutputs . ix i

consuming :: Item -> Recipe -> Bool
consuming i = has $ fTransfer . fInputs . ix i

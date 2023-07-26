module Tech.Recipes where

import Data.Map.Strict qualified as Map
import Tech.Machines (assembler, planter, thresher)
import Tech.Recipes.Assembler qualified as A
import Tech.Recipes.Planter qualified as P
import Tech.Recipes.Thresher qualified as T
import Tech.Types
import Control.Lens (preview, ix)

type Recipes = Map Machine (Map RecipeIdentifier Recipe)

builtinRecipes :: Recipes
builtinRecipes =
  Map.fromList
    [ (assembler, A.recipes)
    , (planter, P.recipes)
    , (thresher, T.recipes)
    ]

findRecipeByKey :: Recipes -> RecipeKey -> Maybe Recipe
findRecipeByKey recipes (RecipeKey m rid) = preview (ix m . ix rid) recipes

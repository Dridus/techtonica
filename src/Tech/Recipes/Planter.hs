module Tech.Recipes.Planter where

import Control.Lens (view)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (NominalDiffTime)
import Tech.Items qualified as I
import Tech.Machines (planter)
import Tech.Types hiding (recipe)

recipe :: RecipeIdentifier -> NominalDiffTime -> Transfer Quantity -> Recipe
recipe rid = Recipe (RecipeKey planter rid)

recipe' :: (Item, Quantity) -> NominalDiffTime -> Image Quantity -> Recipe
recipe' prod@(i, _) time ins = recipe (RecipeIdentifier . unItem $ i) time (ins :=>-: prod)

silverthorn, kindlevine :: Recipe
silverthorn = recipe' (I.silverthorn, 1) 130.0 (One (I.silverthornSeed, 1))
kindlevine = recipe' (I.kindlevine, 1) 130.0 (One (I.kindlevineSeed, 1))

recipes :: Map RecipeIdentifier Recipe
recipes =
  Map.fromList . fmap (view (key . identifier) &&& id) $
    [ silverthorn
    , kindlevine
    ]

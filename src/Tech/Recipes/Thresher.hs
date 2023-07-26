module Tech.Recipes.Thresher where

import Control.Lens (view)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (NominalDiffTime)
import Tech.Items qualified as I
import Tech.Machines (thresher)
import Tech.Types hiding (recipe)

recipe :: RecipeIdentifier -> NominalDiffTime -> Transfer Quantity -> Recipe
recipe rid = Recipe (RecipeKey thresher rid)

recipe' :: (Item, Quantity) -> NominalDiffTime -> Image Quantity -> Recipe
recipe' src@(i, _) time outs = recipe (RecipeIdentifier . unItem $ i) time (src :->=: outs)

silverthorn, silverthornBuds :: Recipe
silverthorn = recipe' (I.silverthorn, 1) 5.75 (Pair (I.silverthornSeed, 1) (I.silverthornBuds, 4))
silverthornBuds = recipe' (I.silverthornBuds, 1) 3.00 (Pair (I.silverthornExtract, 4) (I.plantmatter, 5))

kindlevine, kindlevineSticks :: Recipe
kindlevine = recipe' (I.kindlevine, 1) 6.00 (Pair (I.kindlevineSeed, 1) (I.kindlevineSticks, 4))
kindlevineSticks = recipe' (I.kindlevineSticks, 1) 3.00 (Pair (I.kindlevineExtract, 4) (I.plantmatterFiber, 4))

recipes :: Map RecipeIdentifier Recipe
recipes =
  Map.fromList . fmap (view (key . identifier) &&& id) $
    [ silverthorn
    , silverthornBuds
    , kindlevine
    , kindlevineSticks
    ]

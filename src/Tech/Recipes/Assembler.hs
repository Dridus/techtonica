module Tech.Recipes.Assembler where

import Control.Lens (each, over, view)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (NominalDiffTime)
import Tech.Items qualified as I
import Tech.Machines (assembler)
import Tech.Types hiding (recipe)

assemblerToHand :: Recipe -> Recipe
assemblerToHand = over (transfer . outputs . each) (/ 2) . over cycleTime (/ 4)

recipe :: RecipeIdentifier -> NominalDiffTime -> Transfer Quantity -> Recipe
recipe rid = Recipe (RecipeKey assembler rid)

recipe' :: (Item, Quantity) -> NominalDiffTime -> Image Quantity -> Recipe
recipe' prod@(i, _) time ins = recipe (RecipeIdentifier . unItem $ i) time (ins :=>-: prod)

copperComponents, copperFrame :: Recipe
copperComponents = recipe' (I.copperComponents, 4) 8.0 (One (I.copperIngot, 3))
copperFrame = recipe' (I.copperFrame, 2) 12.0 (One (I.copperIngot, 4))

ironComponents, ironFrame :: Recipe
ironComponents = recipe' (I.ironComponents, 2) 4.0 (One (I.ironIngot, 1))
ironFrame = recipe' (I.ironFrame, 2) 12.0 (One (I.ironIngot, 4))

recipes :: Map RecipeIdentifier Recipe
recipes =
  Map.fromList . fmap (view (key . identifier) &&& id) $
    [ copperComponents
    , copperFrame
    , ironComponents
    , ironFrame
    ]

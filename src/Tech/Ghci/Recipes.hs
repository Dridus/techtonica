module Tech.Ghci.Recipes where

import Control.Lens (andOf, both, has, ix, over, preview, set)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime)
import Prettyprinter (viaShow, vsep, (<+>))
import Tech.Ghci.State (currentItems, currentRecipes, items, recipes, updateState)
import Tech.Ghci.Utils (printVerify, putDocLn)
import Tech.Pretty (kw, ppItem, ppLoadError, ppLoadWarning, ppMachine, ppRecipe)
import Tech.Recipes (filterRecipes, findRecipeByKey, insertRecipe)
import Tech.Store (loadRecipesFile, storeRecipesFile)
import Tech.Types
import Tech.Verify qualified as Verify
import Prelude hiding (state)

loadRecipes :: FilePath -> IO ()
loadRecipes fp =
  loadRecipesFile fp >>= \case
    Left err -> putDocLn . ppLoadError $ err
    Right (warns, (its, rs)) -> do
      putDocLn . vsep . fmap ppLoadWarning $ warns
      updateState (kw "loadRecipes" <+> viaShow fp) $ set items its . set recipes rs

saveRecipes :: FilePath -> IO ()
saveRecipes fp = storeRecipesFile fp =<< ((,) <$> currentItems <*> currentRecipes)

findRecipe :: Machine -> RecipeIdentifier -> IO Recipe
findRecipe m rid = do
  rs <- currentRecipes
  case findRecipeByKey rs (RecipeKey m rid) of
    Just r -> pure r
    Nothing -> fail "recipe not found"

findRecipes :: (Recipe -> Bool) -> IO ()
findRecipes p = putDocLn . vsep . fmap ppRecipe . filterRecipes p =<< currentRecipes

listAllRecipes :: IO ()
listAllRecipes = putDocLn . vsep . fmap f . Map.toList =<< currentRecipes
 where
  f (_, rs) = vsep $ fmap ppRecipe (Map.elems rs)

listRecipes :: Machine -> IO ()
listRecipes m =
  putDocLn
    . vsep
    . fmap ppRecipe
    . Map.elems
    . fromMaybe mempty
    . Map.lookup m
    =<< currentRecipes

verifyRecipe :: Recipe -> IO (Bool, Bool)
verifyRecipe r = do
  knownItems <- currentItems
  printVerify $ Verify.verifyRecipe knownItems r

addRecipe :: Machine -> RecipeIdentifier -> NominalDiffTime -> Transfer Quantity -> IO ()
addRecipe m rid ctime txfr = do
  whenM (has (ix m . ix rid) <$> currentRecipes) $
    fail "recipe already exists. maybe editRecipe?"
  whenM (andOf both <$> verifyRecipe r) $
    updateState (kw "addRecipe" <+> ppMachine m <+> viaShow rid) $
      over recipes (insertRecipe r)
 where
  r = Recipe (RecipeKey m rid) ctime txfr

editRecipe :: Machine -> RecipeIdentifier -> (Recipe -> Recipe) -> IO ()
editRecipe m rid f = do
  r <- maybe (fail "recipe not found") pure . preview (ix m . ix rid) =<< currentRecipes
  let r' = f r
  whenM (andOf both <$> verifyRecipe r') $
    updateState (kw "editRecipe" <+> ppMachine m <+> viaShow rid) $
      over recipes (insertRecipe r')

delRecipe :: Machine -> RecipeIdentifier -> IO ()
delRecipe m rid =
  updateState (kw "delRecipe" <+> ppMachine m <+> viaShow rid) $
    over recipes (Map.adjust (Map.delete rid) m)

listItems :: IO ()
listItems = putDocLn . vsep . fmap ppItem . Set.toList =<< currentItems

addItem :: Item -> IO ()
addItem i = updateState (kw "addItem" <+> viaShow i) $ over items (Set.insert i)

delItem :: Item -> IO ()
delItem i = updateState (kw "delItem" <+> viaShow i) $ over items (Set.delete i)

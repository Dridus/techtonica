module Tech.Ghci.FactoryEnv where

import Control.Lens (andOf, at, both, each, has, hasn't, ix, over, preview, set, to, toListOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime)
import Prettyprinter (viaShow, vsep, (<+>))
import Tech.Ghci.State (currentFactoryEnv, currentItems, currentMachines, currentRecipes, fFactoryEnv, updateState)
import Tech.Ghci.Utils (printVerify, putDocLn)
import Tech.Pretty (kw, ppItem, ppLoadError, ppLoadWarning, ppMachine, ppMachineIdentifier, ppRecipe)
import Tech.Recipes (filterRecipes, insertRecipe)
import Tech.Store (loadEnvFile, storeEnvFile)
import Tech.Types
import Tech.Verify qualified as Verify
import Prelude hiding (state)

loadEnv :: MonadIO m => FilePath -> m ()
loadEnv fp =
  liftIO (loadEnvFile fp) >>= \case
    Left err -> putDocLn . ppLoadError $ err
    Right (warns, fenv) -> do
      putDocLn . vsep . fmap ppLoadWarning $ warns
      updateState (kw "loadEnv" <+> viaShow fp) $ set fFactoryEnv fenv

saveEnv :: MonadIO m => FilePath -> m ()
saveEnv fp = liftIO . storeEnvFile fp =<< currentFactoryEnv

listItems :: MonadIO m => m ()
listItems = putDocLn . vsep . fmap ppItem . Set.toList =<< currentItems

addItem :: MonadIO m => Item -> m ()
addItem i = updateState (kw "addItem" <+> viaShow i) $ over fItems (Set.insert i)

delItem :: MonadIO m => Item -> m ()
delItem i = updateState (kw "delItem" <+> viaShow i) $ over fItems (Set.delete i)

listMachines :: MonadIO m => m ()
listMachines = putDocLn . vsep . toListOf (each . to ppMachine) =<< currentMachines

addMachine :: (MonadFail m, MonadIO m) => MachineIdentifier -> m ()
addMachine = (`addMachine'` 1)

addMachine' :: (MonadFail m, MonadIO m) => MachineIdentifier -> Rational -> m ()
addMachine' mid par = do
  whenM (has (ix mid) <$> currentMachines) $
    fail "machine exists. maybe editMachine?"
  updateState (kw "addMachine" <+> ppMachineIdentifier mid) $
    set (fMachines . at mid) (Just $ Machine mid par)

editMachine :: (MonadFail m, MonadIO m) => MachineIdentifier -> (Machine -> Machine) -> m ()
editMachine mid f = do
  m <- maybe (fail "machine not found") pure . preview (ix mid) =<< currentMachines
  let m' = f m
  updateState (kw "editMachine" <+> ppMachineIdentifier mid) $
    set (fMachines . ix mid) m'

delMachine :: MonadIO m => MachineIdentifier -> m ()
delMachine mid =
  updateState (kw "delMachine" <+> ppMachineIdentifier mid) $
    set (fMachines . at mid) Nothing . set (fRecipes . at mid) Nothing

findRecipes :: MonadIO m => (Recipe -> Bool) -> m ()
findRecipes p = putDocLn . vsep . fmap ppRecipe . filterRecipes p =<< currentRecipes

listAllRecipes :: MonadIO m => m ()
listAllRecipes = putDocLn . vsep . fmap f . Map.toList =<< currentRecipes
 where
  f (_, rs) = vsep $ fmap ppRecipe (Map.elems rs)

listRecipes :: MonadIO m => MachineIdentifier -> m ()
listRecipes mid =
  putDocLn . vsep . toListOf (ix mid . each . to ppRecipe) =<< currentRecipes

verifyRecipe :: MonadIO m => Recipe -> m (Bool, Bool)
verifyRecipe r = do
  knownItems <- currentItems
  printVerify $ Verify.verifyRecipe knownItems r

addRecipe
  :: (MonadFail m, MonadIO m)
  => MachineIdentifier
  -> RecipeIdentifier
  -> NominalDiffTime
  -> Transfer Quantity
  -> m ()
addRecipe mid rid ctime txfr = do
  whenM (hasn't (ix mid) <$> currentMachines) $
    fail "no such machine?"
  whenM (has (ix mid . ix rid) <$> currentRecipes) $
    fail "recipe already exists. maybe editRecipe?"
  whenM (andOf both <$> verifyRecipe r) $
    updateState (kw "addRecipe" <+> ppMachineIdentifier mid <+> viaShow rid) $
      over fRecipes (insertRecipe r)
 where
  r = Recipe (RecipeKey mid rid) ctime txfr

editRecipe
  :: (MonadFail m, MonadIO m)
  => MachineIdentifier
  -> RecipeIdentifier
  -> (Recipe -> Recipe)
  -> m ()
editRecipe mid rid f = do
  r <- maybe (fail "recipe not found") pure . preview (ix mid . ix rid) =<< currentRecipes
  let r' = f r
  whenM (andOf both <$> verifyRecipe r') $
    updateState (kw "editRecipe" <+> ppMachineIdentifier mid <+> viaShow rid) $
      set (fRecipes . ix mid . ix rid) r'

delRecipe :: MonadIO m => MachineIdentifier -> RecipeIdentifier -> m ()
delRecipe mid rid =
  updateState (kw "delRecipe" <+> ppMachineIdentifier mid <+> viaShow rid) $
    set (fRecipes . ix mid . at rid) Nothing

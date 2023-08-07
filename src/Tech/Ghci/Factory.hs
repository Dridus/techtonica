module Tech.Ghci.Factory where

import Control.Lens (over, set, view, _3, ix, preview)
import Data.Graph.Inductive (Node)
import Data.Graph.Inductive qualified as Gr
import Data.Text.Lazy.IO qualified as TLIO
import Prettyprinter (annotate, pretty, viaShow, vsep, (<+>))
import Prettyprinter.Render.Terminal (Color (Green), color)
import Tech.Ghci.State (currentFactory, currentRecipes, fFactory, updateState, currentMachines, withFactoryEnv)
import Tech.Ghci.Utils (printVerify, putDocLn)
import Tech.Mermaid (graphFactorySt)
import Tech.Pretty (kw, ppFactorySt, ppLoadError, ppLoadWarning, ppQuantity, ppRecipeKey)
import Tech.Store (loadFactoryFile, storeFactoryFile)
import Tech.Types
import Tech.Verify (verifyFactorySt)
import Prelude hiding (state)

printFactory :: MonadIO m => m ()
printFactory = putDocLn . ppFactorySt =<< currentFactory

graphFactory :: MonadIO m => FilePath -> m ()
graphFactory fp = liftIO . TLIO.writeFile fp . graphFactorySt =<< currentFactory

saveFactory :: MonadIO m => FilePath -> m ()
saveFactory fp =
  withFactoryEnv $ storeFactoryFile fp =<< currentFactory

loadFactory :: MonadIO m => FilePath -> m ()
loadFactory fp = withFactoryEnv $ do
  loadFactoryFile fp >>= \case
    Left err -> putDocLn . ppLoadError $ err
    Right (warns, factSt) -> do
      putDocLn . vsep . fmap ppLoadWarning $ warns
      updateState (kw "loadFactory" <+> viaShow fp) $
        set fFactory factSt

setFactory :: MonadIO m => FactorySt -> m ()
setFactory = updateState (kw "setFactory") . set fFactory

clearFactory :: MonadIO m => m ()
clearFactory = updateState (kw "clearFactory") $ set fFactory Gr.empty

verifyFactory :: MonadIO m => m ()
verifyFactory =
  currentFactory >>= printVerify . verifyFactorySt >>= \case
    (True, True) -> putDocLn $ annotate (color Green) "Verify OK!"
    _ -> pure ()

addCluster :: (MonadFail m, MonadIO m) => MachineIdentifier -> RecipeIdentifier -> Quantity -> m Node
addCluster mid rid qty = do
  m <- maybe (fail "unknown machine") pure . preview (ix mid) =<< currentMachines
  r <- maybe (fail "recipe not known") pure . preview (ix mid . ix rid) =<< currentRecipes
  let c =
        ClusterSt
          { _clusterSt_recipe = r
          , _clusterSt_machine = m
          , _clusterSt_quantity = qty
          }
  gin <- currentFactory
  let n = newNode gin
  updateState (kw "addCluster" <+> ppRecipeKey (view (fRecipe . fKey) c) <+> ppQuantity qty) $
    over fFactory (Gr.insNode (n, c))
  pure n

editCluster :: (MonadFail m, MonadIO m) => Node -> (ClusterSt -> ClusterSt) -> m ()
editCluster n f = do
  g <- currentFactory
  let (mctx, g') = Gr.match n g
  ctx <- maybe (fail "Invalid node") pure mctx
  let ctx' = over _3 f ctx
  updateState (kw "editCluster" <+> pretty n) $
    set fFactory (ctx' Gr.& g')

delCluster :: MonadIO m => Node -> m ()
delCluster n =
  updateState (kw "delCluster" <+> pretty n) $ over fFactory (Gr.delNode n)

addBelt :: MonadIO m => Node -> Node -> Item -> m ()
addBelt np ns i =
  updateState (kw "addBelt" <+> pretty np <+> pretty ns <+> viaShow i) $
    over fFactory (Gr.insEdge (np, ns, BeltSt i))

delBelt :: MonadIO m => Node -> Node -> Item -> m ()
delBelt np ns i =
  updateState (kw "delBelt" <+> pretty np <+> pretty ns <+> viaShow i) $
    over fFactory (Gr.delLEdge (np, ns, BeltSt i))

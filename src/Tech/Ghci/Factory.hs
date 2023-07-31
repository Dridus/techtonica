module Tech.Ghci.Factory where

import Control.Lens (over, set, view, _3)
import Data.Graph.Inductive (Node)
import Data.Graph.Inductive qualified as Gr
import Prettyprinter (annotate, pretty, viaShow, vsep, (<+>))
import Prettyprinter.Render.Terminal (Color (Green), color)
import Tech.Ghci.State (currentFactory, currentRecipes, fFactory, updateState)
import Tech.Ghci.Utils (printVerify, putDocLn)
import Tech.Pretty (kw, ppFactorySt, ppLoadError, ppLoadWarning, ppQuantity, ppRecipeKey)
import Tech.Store (loadFactoryFile, storeFactoryFile)
import Tech.Types
import Tech.Verify (verifyFactorySt)
import Prelude hiding (state)

printFactory :: IO ()
printFactory = putDocLn . ppFactorySt =<< currentFactory

saveFactory :: FilePath -> IO ()
saveFactory fp =
  join $ storeFactoryFile <$> currentRecipes <*> pure fp <*> currentFactory

loadFactory :: FilePath -> IO ()
loadFactory fp = do
  knownRecipes <- currentRecipes
  loadFactoryFile knownRecipes fp >>= \case
    Left err -> putDocLn . ppLoadError $ err
    Right (warns, factSt) -> do
      putDocLn . vsep . fmap ppLoadWarning $ warns
      updateState (kw "loadFactory" <+> viaShow fp) $
        set fFactory factSt

setFactory :: FactorySt -> IO ()
setFactory = updateState (kw "setFactory") . set fFactory

clearFactory :: IO ()
clearFactory = updateState (kw "clearFactory") $ set fFactory Gr.empty

verifyFactory :: IO ()
verifyFactory =
  currentFactory >>= printVerify . verifyFactorySt >>= \case
    (True, True) -> putDocLn $ annotate (color Green) "Verify OK!"
    _ -> pure ()

addCluster :: IO Recipe -> Quantity -> IO Node
addCluster recipeIO qty = do
  c <- ClusterSt <$> recipeIO <*> pure qty
  gin <- currentFactory
  let n = newNode gin
  updateState (kw "addCluster" <+> ppRecipeKey (view (fRecipe . fKey) c) <+> ppQuantity qty) $
    over fFactory (Gr.insNode (n, c))
  pure n

editCluster :: Node -> (ClusterSt -> ClusterSt) -> IO ()
editCluster n f = do
  g <- currentFactory
  let (mctx, g') = Gr.match n g
  ctx <- maybe (fail "Invalid node") pure mctx
  let ctx' = over _3 f ctx
  updateState (kw "editCluster" <+> pretty n) $
    set fFactory (ctx' Gr.& g')

delCluster :: Node -> IO ()
delCluster n =
  updateState (kw "delCluster" <+> pretty n) $ over fFactory (Gr.delNode n)

addBelt :: Node -> Node -> Item -> IO ()
addBelt np ns i =
  updateState (kw "addBelt" <+> pretty np <+> pretty ns <+> viaShow i) $
    over fFactory (Gr.insEdge (np, ns, BeltSt i))

delBelt :: Node -> Node -> Item -> IO ()
delBelt np ns i =
  updateState (kw "delBelt" <+> pretty np <+> pretty ns <+> viaShow i) $
    over fFactory (Gr.delLEdge (np, ns, BeltSt i))

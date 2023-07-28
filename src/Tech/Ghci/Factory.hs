module Tech.Ghci.Factory where

import Control.Lens (view, _3, over, set)
import Data.Graph.Inductive qualified as Gr
import Prelude hiding (state)
import Prettyprinter (annotate, vsep)
import Prettyprinter.Render.Terminal (Color (Green), color)
import Tech.Ghci.State (currentState, factory, currentFactory, currentRecipes, recipes)
import Tech.Pretty (ppLoadError, ppLoadWarning, ppFactorySt)
import Tech.Store (loadFactoryFile, storeFactoryFile)
import Tech.Types
import Tech.Verify (verifyFactorySt)
import Data.Graph.Inductive (Node)
import Tech.Ghci.Utils (putDocLn, printVerify)

printFactory :: IO ()
printFactory = putDocLn . ppFactorySt . view factory =<< readIORef currentState

saveFactory :: FilePath -> IO ()
saveFactory fp = do
  state <- readIORef currentState
  storeFactoryFile (view recipes state) fp (view factory state)

loadFactory :: FilePath -> IO ()
loadFactory fp = do
  knownRecipes <- currentRecipes
  loadFactoryFile knownRecipes fp >>= \case
    Left err -> putDocLn . ppLoadError $ err
    Right (warns, factSt) -> do
      putDocLn . vsep . fmap ppLoadWarning $ warns
      setFactory factSt

setFactory :: FactorySt -> IO ()
setFactory = modifyIORef' currentState . set factory

clearFactory :: IO ()
clearFactory = setFactory Gr.empty

verifyFactory :: IO ()
verifyFactory =
  currentFactory >>= printVerify . verifyFactorySt >>= \case
    (True, True) -> putDocLn $ annotate (color Green) "Verify OK!"
    _ -> pure ()


addCluster :: IO Recipe -> Quantity -> IO Node
addCluster recipeIO qty = do
  c <- ClusterSt <$> recipeIO <*> pure qty
  gin <- currentFactory
  let n = if Gr.isEmpty gin then 1 else succ . snd $ Gr.nodeRange gin
  n <$ setFactory (Gr.insNode (n, c) gin)

editCluster :: Node -> (ClusterSt -> ClusterSt) -> IO ()
editCluster n f = do
  g <- currentFactory
  let (mctx, g') = Gr.match n g
  ctx <- maybe (fail "Invalid node") pure mctx
  let ctx' = over _3 f ctx
  setFactory $ ctx' Gr.& g'

delCluster :: Node -> IO ()
delCluster n = modifyIORef' currentState . over factory $ Gr.delNode n

addBelt :: Node -> Node -> Item -> IO ()
addBelt np ns i = modifyIORef' currentState . over factory $ Gr.insEdge (np, ns, BeltSt i)

delBelt :: Node -> Node -> Item -> IO ()
delBelt np ns i = modifyIORef' currentState . over factory $ Gr.delLEdge (np, ns, BeltSt i)

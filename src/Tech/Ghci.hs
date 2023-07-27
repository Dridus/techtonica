module Tech.Ghci (
  module Export,
  putDocLn,
  currentFactory,
  currentRecipes,
  printFactory,
  saveFactory,
  loadFactory,
  setFactory,
  clearFactory,
  estimateFactory,
  verifyFactory,
  addCluster,
  editCluster,
  delCluster,
  addBelt,
  delBelt,
  findRecipe,
  saveRecipes,
  loadRecipes,
  listAllRecipes,
  listRecipes,
  addRecipe,
  delRecipe,
  listItems,
  addItem,
  delItem,
) where

import Control.Lens as Export
import Data.Graph.Inductive (Node)
import Data.Graph.Inductive qualified as Gr
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime)
import Prettyprinter (vsep)
import Prettyprinter qualified as Pp
import Prettyprinter.Render.Terminal as Export (putDoc)
import Prettyprinter.Render.Terminal qualified as PpT
import System.IO.Unsafe (unsafePerformIO)
import Tech.Machines as Export
import Tech.Planner as Export
import Tech.Pretty as Export
import Tech.Recipes (Recipes, findRecipeByKey)
import Tech.Store qualified as Store
import Tech.Types as Export
import Tech.Verify as Export (verifyFactorySt)

putDocLn :: Pp.Doc PpT.AnsiStyle -> IO ()
putDocLn d = putDoc d >> putStrLn ""

{-# NOINLINE currentFactory #-}
currentFactory :: IORef FactorySt
currentFactory = unsafePerformIO (newIORef Gr.empty)

{-# NOINLINE currentItems #-}
currentItems :: IORef (Set Item)
currentItems = unsafePerformIO (newIORef mempty)

{-# NOINLINE currentRecipes #-}
currentRecipes :: IORef Recipes
currentRecipes = unsafePerformIO (newIORef mempty)

printFactory :: IO ()
printFactory = putDocLn . ppFactorySt =<< readIORef currentFactory

saveFactory :: FilePath -> IO ()
saveFactory fp = do
  knownRecipes <- readIORef currentRecipes
  factSt <- readIORef currentFactory
  Store.storeFactoryFile knownRecipes fp factSt

loadFactory :: FilePath -> IO ()
loadFactory fp = do
  knownRecipes <- readIORef currentRecipes
  Store.loadFactoryFile knownRecipes fp >>= \case
    Left err -> putDocLn . ppLoadError $ err
    Right (warns, factSt) -> do
      putDocLn . vsep . fmap ppLoadWarning $ warns
      setFactory factSt

setFactory :: FactorySt -> IO ()
setFactory = writeIORef currentFactory

clearFactory :: IO ()
clearFactory = setFactory Gr.empty

estimateFactory :: IO ()
estimateFactory = putDocLn . ppFactoryDy . estimate =<< readIORef currentFactory

verifyFactory :: IO ()
verifyFactory = void . printVerify' =<< readIORef currentFactory

printVerify' :: FactorySt -> IO Bool
printVerify' fact = do
  putDocLn . Pp.vsep $
    (ppVerifyWarning <$> toList warnings)
      <> (ppVerifyError <$> toList errors)
      <> [disposition]
  pure (null errors)
 where
  (warnings, res) = verifyFactorySt fact
  errors = fromMaybe mempty $ preview _Left res
  disposition = case (null errors, null warnings) of
    (True, True) ->
      Pp.annotate
        (PpT.color PpT.Green)
        "Verify OK!"
        <> Pp.line
    (True, False) ->
      Pp.annotate
        (PpT.color PpT.Yellow)
        ("Verified with" Pp.<+> Pp.pretty (length warnings) Pp.<+> "warning(s).")
        <> Pp.line
    (False, True) ->
      Pp.annotate
        (PpT.color PpT.Red)
        ("Verify failed with" Pp.<+> Pp.pretty (length errors) Pp.<+> "error(s).")
        <> Pp.line
    (False, False) ->
      Pp.annotate
        (PpT.color PpT.Red)
        ( "Verify failed with"
            Pp.<+> Pp.pretty (length errors)
            Pp.<+> "error(s) and"
            Pp.<+> Pp.pretty (length warnings)
            Pp.<+> "warning(s)."
        )
        <> Pp.line

addCluster :: IO Recipe -> Quantity -> IO Node
addCluster recipeIO qty = do
  c <- ClusterSt <$> recipeIO <*> pure qty
  gin <- readIORef currentFactory
  let n = if Gr.isEmpty gin then 1 else succ . snd $ Gr.nodeRange gin
  n <$ writeIORef currentFactory (Gr.insNode (n, c) gin)

editCluster :: Node -> (ClusterSt -> ClusterSt) -> IO ()
editCluster n f = do
  g <- readIORef currentFactory
  let (mctx, g') = Gr.match n g
  ctx <- maybe (fail "Invalid node") pure mctx
  let ctx' = over _3 f ctx
  writeIORef currentFactory $! ctx' Gr.& g'

delCluster :: Node -> IO ()
delCluster n = modifyIORef' currentFactory $ Gr.delNode n

addBelt :: Node -> Node -> Item -> IO ()
addBelt np ns i = modifyIORef' currentFactory $ Gr.insEdge (np, ns, BeltSt i)

delBelt :: Node -> Node -> Item -> IO ()
delBelt np ns i = modifyIORef' currentFactory $ Gr.delLEdge (np, ns, BeltSt i)

loadRecipes :: FilePath -> IO ()
loadRecipes fp =
  Store.loadRecipesFile fp >>= \case
    Left err -> putDocLn . ppLoadError $ err
    Right (warns, (items, recipes)) -> do
      putDocLn . vsep . fmap ppLoadWarning $ warns
      writeIORef currentItems items
      writeIORef currentRecipes recipes

saveRecipes :: FilePath -> IO ()
saveRecipes fp = Store.storeRecipesFile fp =<< ((,) <$> readIORef currentItems <*> readIORef currentRecipes)

findRecipe :: Machine -> RecipeIdentifier -> IO Recipe
findRecipe m rid = do
  recipes <- readIORef currentRecipes
  case findRecipeByKey recipes (RecipeKey m rid) of
    Just r -> pure r
    Nothing -> fail "recipe not found"

listAllRecipes :: IO ()
listAllRecipes = putDocLn . vsep . fmap f . Map.toList =<< readIORef currentRecipes
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
    =<< readIORef currentRecipes

addRecipe :: Machine -> RecipeIdentifier -> NominalDiffTime -> Transfer Quantity -> IO ()
addRecipe m rid ctime txfr = do
  items <- readIORef currentItems
  let usedItems =
        Set.fromList
          . toListOf (to (view inputs &&& view outputs) . both . ifolded . asIndex)
          $ txfr
  let unregisteredItems = Set.difference usedItems items
  unless (Set.null unregisteredItems) $ do
    putDocLn . Pp.vsep $
      Pp.annotate (PpT.color PpT.Red) "One or more used items are not registered:"
        : (ppItem <$> Set.toList unregisteredItems)

  modifyIORef' currentRecipes $ Map.insertWith (<>) m (Map.singleton rid r)
 where
  r = Recipe (RecipeKey m rid) ctime txfr

delRecipe :: Machine -> RecipeIdentifier -> IO ()
delRecipe m rid = modifyIORef' currentRecipes $ Map.adjust (Map.delete rid) m

listItems :: IO ()
listItems = putDocLn . vsep . fmap ppItem . Set.toList =<< readIORef currentItems

addItem :: Item -> IO ()
addItem = modifyIORef' currentItems . Set.insert

delItem :: Item -> IO ()
delItem = modifyIORef' currentItems . Set.delete

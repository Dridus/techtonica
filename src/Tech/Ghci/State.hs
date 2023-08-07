module Tech.Ghci.State (
  Generation (..),
  GhciState (..),
  Has_fFactory (..),
  Has_fFactoryEnv (..),
  resetState,
  currentGeneration,
  currentFactory,
  withFactoryEnv,
  currentFactoryEnv,
  currentItems,
  currentMachines,
  currentRecipes,
  updateState,
  history,
  undo,
  redo,
)
where

import Control.Lens (view, _1, _2)
import Control.Lens.TH (makeLensesWith, makeWrapped)
import Data.Graph.Inductive qualified as Gr
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Prettyprinter (Doc, annotate, indent, pretty, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Yellow), color)
import System.IO.Unsafe (unsafePerformIO)
import Tech.Ghci.Utils (putDocLn)
import Tech.LensOptions (techFields)
import Tech.Planner.Propose (Has_fFactory (..))
import Tech.Pretty (errDoc, kw, parens)
import Tech.Types

newtype Generation = Generation {unGeneration :: Int}
deriving newtype instance Enum Generation
deriving newtype instance Eq Generation
deriving newtype instance Ord Generation
deriving stock instance Show Generation
makeWrapped ''Generation

data GhciState = GhciState
  { _ghciState_factory :: FactorySt
  , _ghciState_factoryEnv :: FactoryEnv
  }
deriving stock instance Show GhciState
makeLensesWith techFields ''GhciState
instance Has_fItems GhciState (Set Item) where fItems = fFactoryEnv . fItems
instance Has_fMachines GhciState Machines where fMachines = fFactoryEnv . fMachines
instance Has_fRecipes GhciState Recipes where fRecipes = fFactoryEnv . fRecipes

newState :: GhciState
newState = GhciState Gr.empty (FactoryEnv mempty mempty mempty)

{-# NOINLINE currentStateRef #-}
currentStateRef :: IORef (Generation, GhciState)
currentStateRef = unsafePerformIO (newIORef (Generation 0, newState))

{-# NOINLINE stateHistoryRef #-}
stateHistoryRef :: IORef (Seq (Doc AnsiStyle, Generation, GhciState))
stateHistoryRef = unsafePerformIO (newIORef mempty)

{-# NOINLINE undoneStateRef #-}
undoneStateRef :: IORef (Seq (Doc AnsiStyle, Generation, GhciState))
undoneStateRef = unsafePerformIO (newIORef mempty)

resetState :: MonadIO m => m ()
resetState = writeIORef currentStateRef (Generation 0, newState)

currentGeneration :: MonadIO m => m Generation
currentGeneration = view _1 <$> readIORef currentStateRef

currentFactory :: MonadIO m => m FactorySt
currentFactory = view (_2 . fFactory) <$> readIORef currentStateRef

currentFactoryEnv :: MonadIO m => m FactoryEnv
currentFactoryEnv = view (_2 . fFactoryEnv) <$> readIORef currentStateRef

withFactoryEnv
  :: MonadIO m
  => (forall m'. (MonadIO m', MonadReader FactoryEnv m') => m' a)
  -> m a
withFactoryEnv act = runReaderT act =<< currentFactoryEnv

currentItems :: MonadIO m => m (Set Item)
currentItems = view (_2 . fItems) <$> readIORef currentStateRef

currentMachines :: MonadIO m => m Machines
currentMachines = view (_2 . fMachines) <$> readIORef currentStateRef

currentRecipes :: MonadIO m => m Recipes
currentRecipes = view (_2 . fRecipes) <$> readIORef currentStateRef

updateState :: MonadIO m => Doc AnsiStyle -> (GhciState -> GhciState) -> m ()
updateState deltaLog f = do
  (g, s) <- readIORef currentStateRef
  let
    g' = succ g
    s' = f s
  writeIORef currentStateRef (g', s')
  writeIORef undoneStateRef mempty
  modifyIORef' stateHistoryRef $ Seq.take 50 . (:<|) (deltaLog, g, s)

ppGeneration :: Generation -> Doc AnsiStyle
ppGeneration = annotate (color Yellow) . pretty . unGeneration

ppHistoryState :: Doc AnsiStyle -> (Doc AnsiStyle, Generation, GhciState) -> Doc AnsiStyle
ppHistoryState relation (deltaLog, generation, _) =
  parens (ppGeneration generation) <+> relation <+> deltaLog

history :: MonadIO m => m ()
history = do
  hist <- readIORef stateHistoryRef
  (g, _) <- readIORef currentStateRef
  undone <- readIORef undoneStateRef
  putDocLn . vsep $
    (ppHistoryState "before" <$> toList (Seq.reverse hist))
      <> [ indent 2 $
            vsep
              [ "↑ undo"
              , parens (ppGeneration g) <+> kw "current state"
              , "↓ redo"
              ]
         ]
      <> (ppHistoryState "after" <$> toList undone)

undo :: MonadIO m => m ()
undo =
  readIORef stateHistoryRef >>= \case
    (deltaLog, g', s') :<| ss -> do
      (g, s) <- readIORef currentStateRef
      writeIORef currentStateRef (g', s')
      modifyIORef' undoneStateRef ((deltaLog, g, s) :<|)
      writeIORef stateHistoryRef ss
    _ -> do
      putDocLn (errDoc "no more to undo!")

redo :: MonadIO m => m ()
redo =
  readIORef undoneStateRef >>= \case
    (deltaLog, g', s') :<| ss -> do
      (g, s) <- readIORef currentStateRef
      writeIORef currentStateRef (g', s')
      modifyIORef' stateHistoryRef ((deltaLog, g, s) :<|)
      writeIORef undoneStateRef ss
    _ -> do
      putDocLn (errDoc "no more to redo!")

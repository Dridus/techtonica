module Tech.Ghci.State (
  GhciState (..),
  Has_factory (..),
  Has_recipes (..),
  Has_items (..),
  resetState,
  currentFactory,
  currentItems,
  currentRecipes,
  updateState,
  printChangelog,
  undo,
  redo,
)
where

import Control.Lens (each, toListOf, view, _1)
import Control.Lens.TH (makeLensesWith, underscoreFields)
import Data.Graph.Inductive qualified as Gr
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Prettyprinter (Doc, annotate, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Magenta, Red), color)
import System.IO.Unsafe (unsafePerformIO)
import Tech.Ghci.Utils (putDocLn)
import Tech.Pretty (kw)
import Tech.Recipes (Recipes)
import Tech.Types

data GhciState = GhciState
  { _ghciState_factory :: FactorySt
  , _ghciState_recipes :: Recipes
  , _ghciState_items :: Set Item
  }
deriving stock instance Show GhciState
makeLensesWith underscoreFields ''GhciState

newState :: GhciState
newState = GhciState Gr.empty mempty mempty

{-# NOINLINE currentState #-}
currentState :: IORef GhciState
currentState = unsafePerformIO (newIORef newState)

{-# NOINLINE stateHistory #-}
stateHistory :: IORef (Seq (Doc AnsiStyle, GhciState))
stateHistory = unsafePerformIO (newIORef mempty)

{-# NOINLINE undoneState #-}
undoneState :: IORef (Seq (Doc AnsiStyle, GhciState))
undoneState = unsafePerformIO (newIORef mempty)

resetState :: IO ()
resetState = writeIORef currentState newState

currentFactory :: IO FactorySt
currentFactory = view factory <$> readIORef currentState

currentItems :: IO (Set Item)
currentItems = view items <$> readIORef currentState

currentRecipes :: IO Recipes
currentRecipes = view recipes <$> readIORef currentState

updateState :: Doc AnsiStyle -> (GhciState -> GhciState) -> IO ()
updateState deltaLog f = do
  s <- readIORef currentState
  let s' = f s
  writeIORef currentState s'
  writeIORef undoneState mempty
  modifyIORef' stateHistory $ Seq.take 50 . (:<|) (deltaLog, s)

printChangelog :: IO ()
printChangelog = do
  history <- readIORef stateHistory
  undone <- readIORef undoneState
  putDocLn . vsep $
    toListOf (each . _1) history
      <> [ "↑ undo"
         , kw "current state"
         , "↓ redo"
         ]
      <> toListOf (each . _1) undone

undo :: IO ()
undo =
  readIORef stateHistory >>= \case
    (deltaLog, s') :<| ss -> do
      s <- readIORef currentState
      writeIORef currentState s'
      modifyIORef' undoneState ((deltaLog, s) :<|)
      writeIORef stateHistory ss
    _ -> do
      putDocLn (annotate (color Red) "no more to undo!")

redo :: IO ()
redo =
  readIORef undoneState >>= \case
    (deltaLog, s') :<| ss -> do
      s <- readIORef currentState
      writeIORef currentState s'
      modifyIORef' stateHistory ((deltaLog, s) :<|)
      writeIORef undoneState ss
    _ -> do
      putDocLn (annotate (color Red) "no more to redo!")

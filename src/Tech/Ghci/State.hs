module Tech.Ghci.State where

import Control.Lens (view)
import Control.Lens.TH (makeLensesWith, underscoreFields)
import Data.Graph.Inductive qualified as Gr
import System.IO.Unsafe (unsafePerformIO)
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

resetState :: IO ()
resetState = writeIORef currentState newState

currentFactory :: IO FactorySt
currentFactory = view factory <$> readIORef currentState

currentItems :: IO (Set Item)
currentItems = view items <$> readIORef currentState

currentRecipes :: IO Recipes
currentRecipes = view recipes <$> readIORef currentState

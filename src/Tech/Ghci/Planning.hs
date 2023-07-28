module Tech.Ghci.Planning where

import Tech.Ghci.State (currentFactory)
import Tech.Ghci.Utils (putDocLn)
import Tech.Planner (estimate)
import Tech.Pretty (ppFactoryDy)

estimateFactory :: IO ()
estimateFactory = putDocLn . ppFactoryDy . estimate =<< currentFactory

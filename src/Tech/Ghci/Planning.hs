module Tech.Ghci.Planning where

import Control.Lens (each, filteredBy, ix, lengthOf, preview, previews, set, view, _Left, _Right, _head)
import Data.Text.Lazy.IO qualified as TLIO
import Prettyprinter (annotate, indent, pretty, vsep, (<+>))
import Prettyprinter.Render.Terminal (Color (Yellow), color)
import System.IO.Unsafe (unsafePerformIO)
import Tech.Ghci.State (Generation, currentFactory, currentGeneration, currentRecipes, updateState)
import Tech.Ghci.Utils (putDocLn)
import Tech.Mermaid (graphFactoryDy)
import Tech.Planner.Estimate (estimate)
import Tech.Planner.Propose (Proposal, ProposalConstraints, Proposals, fFactory, fResult, factoryStFromProp, propose)
import Tech.Pretty (errDoc, kw, ppFactoryDy, ppFactorySt, ppProposal)
import Tech.Types

estimateFactory :: IO ()
estimateFactory = putDocLn . ppFactoryDy . estimate =<< currentFactory

graphEstimateFactory :: FilePath -> IO ()
graphEstimateFactory fp = TLIO.writeFile fp . graphFactoryDy . estimate =<< currentFactory

{-# NOINLINE currentProposalsRef #-}
currentProposalsRef :: IORef (Maybe (Generation, Proposals))
currentProposalsRef = unsafePerformIO (newIORef Nothing)

currentProposals :: IO Proposals
currentProposals = do
  gActual <- currentGeneration
  readIORef currentProposalsRef >>= \case
    Nothing -> fail "no proposals!"
    Just (gExpected, _) | gExpected /= gActual -> fail "proposals out of date due to change in factory"
    Just (_, proposals) -> pure proposals

proposeFactory :: Image Rate -> ProposalConstraints Last -> IO ()
proposeFactory needs constraints = do
  generation <- currentGeneration
  recipes <- currentRecipes
  gin <- currentFactory
  let proposals = propose recipes gin needs constraints
  writeIORef currentProposalsRef $ Just (generation, proposals)
  putDocLn . vsep . concat $
    [
      [ "Generated"
          <+> pretty (length proposals)
          <+> "proposals,"
          <+> pretty (lengthOf (each . filteredBy (fResult . _Right)) proposals)
          <+> "successful,"
          <+> pretty (lengthOf (each . filteredBy (fResult . _Left)) proposals)
          <+> "not."
      ]
    , [ ppProposal 1 proposal (maybeProposalEstimate proposal)
      | proposal <- maybeToList $ preview _head proposals
      ]
    , [ "(" <> kw "printProposals" <+> "to see all" <+> pretty (length proposals) <+> "proposals)"
      | length proposals > 1
      ]
    ]

maybeProposalEstimate :: Proposal -> Maybe FactoryDy
maybeProposalEstimate =
  previews (filteredBy (fResult . _Right) . fFactory) (estimate . factoryStFromProp)

printProposal :: Int -> IO ()
printProposal i = do
  proposals <- currentProposals
  case preview (ix (pred i)) proposals of
    Just proposal -> do
      putDocLn $
        ppProposal i proposal (Just . estimate . factoryStFromProp . view fFactory $ proposal)
    Nothing ->
      putDocLn . errDoc $ "no such proposal"

printProposals :: IO ()
printProposals = do
  proposals <- currentProposals
  putDocLn . vsep $
    zip [1 ..] (toList proposals) <&> \(i, p) ->
      ppProposal i p (maybeProposalEstimate p)

acceptProposal :: Int -> IO ()
acceptProposal i = do
  proposals <- currentProposals
  case preview (ix (pred i)) proposals of
    Just proposal -> do
      fact <- currentFactory
      putDocLn . vsep $
        [ annotate (color Yellow) ("Accepting proposal" <+> pretty i)
        , "Existing factory:"
        , indent 2 (ppFactorySt fact)
        , ppProposal i proposal (maybeProposalEstimate proposal)
        ]
      updateState (kw "acceptProposal" <+> pretty i)
        . set fFactory
        . factoryStFromProp
        . view fFactory
        $ proposal
    Nothing ->
      putDocLn . errDoc $ "no such proposal"

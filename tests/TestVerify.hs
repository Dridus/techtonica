module TestVerify where

import Data.Graph.Inductive (mkGraph)
import Data.Graph.Inductive qualified as Gr
import Data.Set qualified as Set
import Prettyprinter (indent, line, vsep)
import Tech.Pretty (errDoc, ppFactorySt, ppVerifyError, ppVerifyWarning)
import Tech.TestFixtures (linearSt, testClusterA1B1, testClusterB1C1, testItemA, testItemB)
import Tech.Types
import Tech.Verify
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import TestUtils (assertFailureDoc)
import Prelude hiding ((&))

tests :: TestTree
tests =
  testGroup
    "Verify"
    [ verifyEmpty
    , verifyOk
    , verifyBeltInput
    , verifyBeltOutput
    ]

infix 4 `shouldVerifyAs`
shouldVerifyAs :: HasCallStack => FactorySt -> (Set VerifyWarning, Set VerifyError) -> Assertion
shouldVerifyAs factSt (expectedWarns, expectedErrs) =
  unless (null failedAssertions) . assertFailureDoc . vsep $
    errDoc "when verifying:"
      : indent 2 (ppFactorySt factSt)
      : failedAssertions
 where
  (actualWarns, fromLeft mempty -> actualErrs) = verifyFactorySt factSt
  missingWarns = Set.difference expectedWarns actualWarns
  unexpectedWarns = Set.difference actualWarns expectedWarns
  missingErrs = Set.difference expectedErrs actualErrs
  unexpectedErrs = Set.difference actualErrs expectedErrs
  renderSet f = vsep . fmap f . Set.toList
  renderWarnings = renderSet ppVerifyWarning
  renderErrors = renderSet ppVerifyError
  failedAssertions =
    concat @[]
      [ [ errDoc "expected warnings not signalled:"
          <> line
          <> indent 2 (renderWarnings missingWarns)
        | not (Set.null missingWarns)
        ]
      , [ errDoc "unexpected warnings signalled:"
          <> line
          <> indent 2 (renderWarnings unexpectedWarns)
        | not (Set.null unexpectedWarns)
        ]
      , [ errDoc "expected errors not signalled:"
          <> line
          <> indent 2 (renderErrors missingErrs)
        | not (Set.null missingErrs)
        ]
      , [ errDoc "unexpected errors signalled:"
          <> line
          <> indent 2 (renderErrors unexpectedErrs)
        | not (Set.null unexpectedErrs)
        ]
      ]

verifyEmpty :: TestTree
verifyEmpty = testCase "empty" $ Gr.empty `shouldVerifyAs` (mempty, mempty)

verifyOk :: TestTree
verifyOk = testCase "ok" $ linearSt `shouldVerifyAs` (mempty, mempty)

-- very difficult to make a graph with an invalid predecessor or success, so skip it

verifyBeltInput :: TestTree
verifyBeltInput =
  testCase "belt input" $
    factSt `shouldVerifyAs` (Set.singleton (BeltItemNotOutputByUp ab badEdge), mempty)
 where
  factSt = mkGraph [ab, ab2, bc] [badEdge, goodEdge]
  ab = (1, testClusterA1B1)
  ab2 = (2, testClusterA1B1)
  bc = (3, testClusterB1C1)
  badEdge = (1, 2, BeltSt testItemA)
  goodEdge = (1, 3, BeltSt testItemB)

verifyBeltOutput :: TestTree
verifyBeltOutput =
  testCase "belt output" $
    factSt `shouldVerifyAs` (Set.singleton (BeltItemNotInputForDown ab2 badEdge), mempty)
 where
  factSt = mkGraph [ab, ab2, bc] [badEdge, goodEdge]
  ab = (1, testClusterA1B1)
  ab2 = (2, testClusterA1B1)
  bc = (3, testClusterB1C1)
  badEdge = (1, 2, BeltSt testItemB)
  goodEdge = (1, 3, BeltSt testItemB)

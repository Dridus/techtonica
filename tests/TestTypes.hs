{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module TestTypes where

import ArbitraryTypes ()
import Control.Lens (over)
import Data.Map.Strict qualified as Map
import Tech.TestFixtures (testItemA)
import Tech.Types
import Test.QuickCheck (mapSize, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)
import TestUtils (peqImageRational, peqRecipe, peqTransferRational)

tests :: TestTree
tests =
  testGroup
    "Types"
    [ testImagePatternSynonyms
    , testSubI
    , testAddI
    , testMulI
    , testSumImage
    , testMulT
    , testDivT
    , testMulR
    , testDivR
    , testShortfall
    , testOverflow
    ]

testImagePatternSynonyms :: TestTree
testImagePatternSynonyms =
  testGroup
    "Image pattern synonyms"
    [ pairConstruct
    , pairDeconstruct2
    , pairDeconstruct3
    , oneConstruct
    , oneDeconstruct1
    , oneDeconstruct2
    ]
 where
  pairConstruct =
    testCase "Pair construct" $
      assertEqual "" (Pair (1 :: Int, 'a') (2, 'b')) (Map.fromList [(1, 'a'), (2, 'b')])
  pairDeconstruct2 =
    testCase "Pair deconstruct" $
      let m = Map.fromList [(1 :: Int, 'a'), (2, 'b')]
      in  case m of
            Pair p1 p2 -> do
              assertEqual "" p1 (1, 'a')
              assertEqual "" p2 (2, 'b')
            _ -> assertFailure $ "Pair p1 p2 should match " <> show m
  pairDeconstruct3 =
    testCase "Pair deconstruct only 2" $
      let m = Map.fromList [(1 :: Int, 'a'), (2, 'b'), (3, 'c')]
      in  case m of
            Pair _ _ -> assertFailure $ "Pair p1 p2 should not match " <> show m
            _ -> pure ()
  oneConstruct =
    testCase "One construct" $
      assertEqual "" (One (1 :: Int, 'a')) (Map.fromList [(1, 'a')])
  oneDeconstruct1 =
    testCase "One deconstruct" $
      let m = Map.fromList [(1 :: Int, 'a')]
      in  case m of
            One p -> assertEqual "" p (1, 'a')
            _ -> assertFailure $ "One p should match " <> show m
  oneDeconstruct2 =
    testCase "One deconstruct only 1" $
      let m = Map.fromList [(1 :: Int, 'a'), (2, 'b')]
      in  case m of
            One _ -> assertFailure $ "One p should not match " <> show m
            _ -> pure ()

testSubI :: TestTree
testSubI = testGroup "subI" [fills, properties]
 where
  fills = testGroup "fill" [leftFill, rightFill]
  leftFill =
    testCase "left fill" $
      assertEqual
        ""
        (mempty `subI` One (testItemA, Quantity 1.0))
        (One (testItemA, Quantity (-1.0)))
  rightFill =
    testCase "right fill" $
      assertEqual
        ""
        (One (testItemA, Quantity 1.0) `subI` mempty)
        (One (testItemA, Quantity 1.0))
  properties = testGroup "properties" [antiCommutativity, addIConcordance, identities]
  antiCommutativity = testProperty "anti-commutativity" $
    \(a :: Image Quantity) b -> a `subI` b `peqImageRational` negateI (b `subI` a)
  addIConcordance = testProperty "addI concordance" $
    \(a :: Image Quantity) b -> a `subI` b `peqImageRational` a `addI` negateI b
  identities = testGroup "identities" [leftIdentity, rightIdentity]
  leftIdentity = testProperty "left identity" $
    \(a :: Image Quantity) -> (0 <$ a) `subI` negateI a `peqImageRational` a
  rightIdentity = testProperty "right identity" $
    \(a :: Image Quantity) -> a `subI` (0 <$ a) `peqImageRational` a

testAddI :: TestTree
testAddI = testGroup "addI" [fills, properties]
 where
  fills = testGroup "fills" [leftFill, rightFill]
  leftFill =
    testCase "left fill" $
      assertEqual
        ""
        (mempty `subI` One (testItemA, Quantity 1.0))
        (One (testItemA, Quantity (-1.0)))
  rightFill =
    testCase "right fill" $
      assertEqual
        ""
        (One (testItemA, Quantity 1.0) `subI` mempty)
        (One (testItemA, Quantity 1.0))
  properties = testGroup "properties" [commutativity, associativity, identities]
  commutativity = testProperty "commutativity" $
    \(a :: Image Quantity) b -> a `addI` b `peqImageRational` b `addI` a
  associativity = testProperty "associativity" $
    \(a :: Image Quantity) b c -> a `addI` (b `addI` c) `peqImageRational` (a `addI` b) `addI` c
  identities = testGroup "identities" [leftIdentity, rightIdentity]
  leftIdentity = testProperty "left identity" $
    \(a :: Image Quantity) -> (0 <$ a) `addI` a `peqImageRational` a
  rightIdentity = testProperty "right identity" $
    \(a :: Image Quantity) -> a `addI` (0 <$ a) `peqImageRational` a

testMulI :: TestTree
testMulI = testGroup "mulI" [properties]
 where
  properties =
    testGroup
      "properties"
      [ identities
      , distributivity
      , zeroProperties
      , negation
      ]
  identities = testGroup "identities" [leftIdentity, rightIdentity]
  leftIdentity = testProperty "left identity" $
    \(a :: Image Quantity) -> 1 `mulI` a `peqImageRational` a
  rightIdentity = testProperty "right identity" $
    \a -> a `mulI` One (testItemA, Quantity 1) `peqImageRational` One (testItemA, Quantity a)
  distributivity = testProperty "distributivity" $
    \a (b :: Image Quantity) c -> a `mulI` (b `addI` c) `peqImageRational` a `mulI` b `addI` a `mulI` c
  zeroProperties = testGroup "identities" [leftZeroProperty, rightZeroProperty]
  leftZeroProperty = testProperty "left zero property" $
    \(a :: Image Quantity) -> 0 `mulI` a `peqImageRational` (Quantity 0 <$ a)
  rightZeroProperty = testProperty "right zero property" $
    \a -> a `mulI` One (testItemA, Quantity 0) `peqImageRational` One (testItemA, Quantity 0)
  negation = testProperty "negation" $
    \(a :: Image Quantity) -> (-1) `mulI` a `peqImageRational` negateI a

testDivI :: TestTree
testDivI = testGroup "divI" [properties]
 where
  properties =
    testGroup
      "properties"
      [ identities
      ]
  identities = testGroup "identities" [leftIdentity, rightIdentity]
  leftIdentity = testProperty "left identity" $
    \(a :: Image Quantity) -> a `divI` 1 `peqImageRational` a
  rightIdentity = testProperty "right identity" $
    \a -> One (testItemA, Quantity 1) `divI` a `peqImageRational` One (testItemA, Quantity a)

testSumImage :: TestTree
testSumImage = testGroup "SumImage" [addIConcordance]
 where
  addIConcordance = testProperty "concordance" . mapSize (const 10) $
    \(a :: [Image Quantity]) ->
      getSumImage (mconcat (SumImage <$> a)) `peqImageRational` foldl' addI mempty a

testMulT :: TestTree
testMulT = testGroup "mulT" [distributivity]
 where
  distributivity = testProperty "distributivity" $
    \a b@(Transfer (i :: Image Quantity) o) ->
      a `mulT` b `peqTransferRational` Transfer (a `mulI` i) (a `mulI` o)

testDivT :: TestTree
testDivT = testGroup "divT" [distributivity]
 where
  distributivity = testProperty "distributivity" $
    \a@(Transfer (i :: Image Quantity) o) b ->
      a `divT` b `peqTransferRational` Transfer (i `divI` b) (o `divI` b)

testMulR :: TestTree
testMulR = testGroup "mulR" [distributivity]
 where
  distributivity = testProperty "distributivity" $
    \(a :: Rational) b -> a `mulR` b `peqRecipe` over fTransfer (a `mulT`) b

testDivR :: TestTree
testDivR = testGroup "divR" [distributivity]
 where
  distributivity = testProperty "distributivity" $
    \a b -> a `divR` b `peqRecipe` over fTransfer (`divT` b) a

testShortfall :: TestTree
testShortfall = testProperty "shortfall" $
  \a b ->
    let c = shortfall (BeltDy (BeltSt testItemA) a b)
    in  if a > b then c === Rate 0 else c === (b - a)

testOverflow :: TestTree
testOverflow = testProperty "overflow" $
  \a b ->
    let c = overflow (BeltDy (BeltSt testItemA) a b)
    in  if a < b then c === Rate 0 else c === (a - b)

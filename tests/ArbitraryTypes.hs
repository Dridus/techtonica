{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryTypes where

import Tech.Types
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Instances.Text ()
import Data.Time.Clock (secondsToNominalDiffTime)

deriving newtype instance Arbitrary Item
deriving newtype instance Arbitrary Machine
deriving newtype instance Arbitrary RecipeIdentifier
deriving newtype instance Arbitrary Quantity
deriving newtype instance Arbitrary Rate

instance Arbitrary q => Arbitrary (Transfer q) where
  arbitrary = Transfer <$> arbitrary <*> arbitrary
instance Arbitrary RecipeKey where
  arbitrary = RecipeKey <$> arbitrary <*> arbitrary
instance Arbitrary Recipe where
  arbitrary = Recipe <$> arbitrary <*> (secondsToNominalDiffTime <$> arbitrary) <*> arbitrary


{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tech.Store.Orphans where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson.TH (deriveJSON)
import Tech.Store.AesonOptions (aesonOptions)
import Tech.Types

deriving newtype instance FromJSON Item
deriving newtype instance FromJSONKey Item
deriving newtype instance ToJSON Item
deriving newtype instance ToJSONKey Item
deriving newtype instance FromJSON MachineIdentifier
deriving newtype instance ToJSON MachineIdentifier
deriving newtype instance FromJSON RecipeIdentifier
deriving newtype instance FromJSONKey RecipeIdentifier
deriving newtype instance ToJSON RecipeIdentifier
deriving newtype instance ToJSONKey RecipeIdentifier
deriving newtype instance FromJSON Quantity
deriving newtype instance ToJSON Quantity
deriving newtype instance FromJSON PerMinute
deriving newtype instance ToJSON PerMinute

deriveJSON aesonOptions ''Machine
deriveJSON aesonOptions ''Transfer
deriveJSON aesonOptions ''RecipeKey
deriveJSON aesonOptions ''Recipe

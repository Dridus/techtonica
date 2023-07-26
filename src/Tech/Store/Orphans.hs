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
deriving newtype instance FromJSON Machine
deriving newtype instance ToJSON Machine
deriving newtype instance FromJSON RecipeIdentifier
deriving newtype instance FromJSONKey RecipeIdentifier
deriving newtype instance ToJSON RecipeIdentifier
deriving newtype instance ToJSONKey RecipeIdentifier
deriving newtype instance FromJSON Quantity
deriving newtype instance ToJSON Quantity
deriving newtype instance FromJSON Rate
deriving newtype instance ToJSON Rate

deriveJSON aesonOptions ''Transfer
deriveJSON aesonOptions ''RecipeKey
deriveJSON aesonOptions ''Recipe

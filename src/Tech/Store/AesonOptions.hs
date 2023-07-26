module Tech.Store.AesonOptions where

import Data.Aeson (Options, defaultOptions, fieldLabelModifier)

aesonOptions :: Options
aesonOptions = defaultOptions {fieldLabelModifier}
 where
  fieldLabelModifier :: String -> String
  fieldLabelModifier =
    dropWhile (== '_') . dropWhile (/= '_') . dropWhile (== '_')

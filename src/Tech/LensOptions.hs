module Tech.LensOptions where

import Control.Lens (over, set, _head)
import Control.Lens.TH (DefName (MethodName), FieldNamer, LensRules, defaultFieldRules, lensField)
import Data.Char (toUpper)
import Language.Haskell.TH (mkName, nameBase)

techFields :: LensRules
techFields = set lensField techNamer defaultFieldRules

techNamer :: FieldNamer
techNamer _ _ (nameBase -> field) = [MethodName (mkName className) (mkName lensName)]
 where
  baseName = dropWhile (== '_') . dropWhile (/= '_') . dropWhile (== '_') $ field
  lensName = 'f' : over _head toUpper baseName
  className = "Has_" <> lensName

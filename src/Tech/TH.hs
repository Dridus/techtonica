module Tech.TH where

import Data.Traversable (for)
import Data.Text (unpack)
import Language.Haskell.TH (DecsQ, mkName, sigD, valD, varP, normalB)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Tech.Types

makeItems :: Text -> DecsQ
makeItems (words -> items) = do
  decs <- for items $ \ item ->
    let
      itemName = mkName (unpack item)
    in sequence
      [ sigD itemName [t| Tech.Types.Item |]
      , valD (varP itemName) (normalB [| Tech.Types.Item $(TH.lift item) |]) []
      ]
  pure (concat decs)
  

module Tech.Ghci.Utils where

import Control.Lens (preview, _Left)
import Data.Set qualified as Set
import Prettyprinter (Doc, SimpleDocStream (SEmpty), annotate, defaultLayoutOptions, layoutPretty, line, pretty, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Red, Yellow), color, renderIO)
import Tech.Pretty (ppVerifyError, ppVerifyWarning)
import Tech.Verify (VerifyError, VerifyWarning)

putDocLn :: Doc AnsiStyle -> IO ()
putDocLn d
  | ls == SEmpty = pure ()
  | otherwise = renderIO stdout ls >> putStrLn ""
 where
  ls = layoutPretty defaultLayoutOptions d

printVerify :: (Set VerifyWarning, Either (Set VerifyError) ()) -> IO (Bool, Bool)
printVerify (warnings, res) = do
  putDocLn . vsep $
    (ppVerifyWarning <$> toList warnings)
      <> (ppVerifyError <$> toList errors)
      <> [disposition]
  pure (null errors, null warnings)
 where
  errors = fromMaybe mempty $ preview _Left res
  disposition = case (Set.null errors, Set.null warnings) of
    (True, True) -> mempty
    (True, False) ->
      annotate
        (color Yellow)
        ("Verified with" <+> pretty (Set.size warnings) <+> "warning(s).")
        <> line
    (False, True) ->
      annotate
        (color Red)
        ("Verify failed with" <+> pretty (Set.size errors) <+> "error(s).")
        <> line
    (False, False) ->
      annotate
        (color Red)
        ( "Verify failed with"
            <+> pretty (Set.size errors)
            <+> "error(s) and"
            <+> pretty (Set.size warnings)
            <+> "warning(s)."
        )
        <> line

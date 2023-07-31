module Tech.Ghci.Utils where

import Control.Lens (preview, _Left)
import qualified Data.Set as Set
import Prettyprinter (
  Doc,
  SimpleDocStream (SEmpty),
  defaultLayoutOptions,
  layoutPretty,
  line,
  pretty,
  vsep,
  (<+>), layoutPageWidth, PageWidth (AvailablePerLine),
 )
import Prettyprinter.Render.Terminal (AnsiStyle, renderIO)
import qualified System.Console.Terminal.Size as TerminalSize
import Tech.Pretty (errDoc, ppVerifyError, ppVerifyWarning, warnDoc)
import Tech.Verify (VerifyError, VerifyWarning)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE pageSize #-}
pageSize :: IORef (Maybe Int)
pageSize = unsafePerformIO $ newIORef Nothing

putDocLn :: Doc AnsiStyle -> IO ()
putDocLn d = do
  width <- readIORef pageSize >>= \ case
    Nothing -> do
      sz <- TerminalSize.size
      let w = maybe 120 TerminalSize.width sz
      w <$ writeIORef pageSize (Just w)
    Just w -> pure w
  let options = defaultLayoutOptions 
        { layoutPageWidth = AvailablePerLine width 1.0 }
  case layoutPretty options d of
    SEmpty -> pure ()
    ls -> renderIO stdout ls >> putStrLn ""

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
      warnDoc $
        ("Verified with" <+> pretty (Set.size warnings) <+> "warning(s).")
          <> line
    (False, True) ->
      errDoc $
        ("Verify failed with" <+> pretty (Set.size errors) <+> "error(s).")
          <> line
    (False, False) ->
      errDoc $
        ( "Verify failed with"
            <+> pretty (Set.size errors)
            <+> "error(s) and"
            <+> pretty (Set.size warnings)
            <+> "warning(s)."
        )
          <> line

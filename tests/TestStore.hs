{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TestStore where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Diff qualified as AesonDiff
import Data.Aeson.Patch (Patch, patchOperations)
import Data.Aeson.Patch qualified as AesonPatch
import Data.Aeson.Pointer (Pointer, formatPointer)
import Data.Aeson.QQ (aesonQQ)
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding qualified as TE
import Data.Yaml qualified as Yaml
import Prettyprinter (Doc, annotate, indent, pretty, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (White), color, colorDull)
import Tech.Pretty (kw, ppFactorySt, ppLoadError, ppLoadWarning)
import Tech.Store
import Tech.TestFixtures (linearSt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import TestUtils (assertBoolDoc, assertFailureDoc, errDoc, factoryStShouldBe)

tests :: TestTree
tests = testGroup "Store" [linear]

renderYaml :: ToJSON a => a -> Doc AnsiStyle
renderYaml = annotate (colorDull White) . pretty . TE.decodeUtf8 . Yaml.encode

renderJson :: ToJSON a => a -> Doc ann
renderJson = pretty . TE.decodeUtf8 . BSL.toStrict . Aeson.encode

ppDiffPointer :: Pointer -> Doc AnsiStyle
ppDiffPointer = annotate (color White) . pretty . formatPointer

ppDiffOperation :: AesonPatch.Operation -> Doc AnsiStyle
ppDiffOperation = \case
  AesonPatch.Add p v ->
    kw "add" <+> ppDiffPointer p <+> kw "=" <+> renderJson v
  AesonPatch.Cpy pf pt ->
    kw "copy" <+> ppDiffPointer pf <+> kw "to" <+> ppDiffPointer pt
  AesonPatch.Mov pf pt ->
    kw "move" <+> ppDiffPointer pf <+> kw "to" <+> ppDiffPointer pt
  AesonPatch.Rem p ->
    kw "remove" <+> ppDiffPointer p
  AesonPatch.Rep p v ->
    kw "replace" <+> ppDiffPointer p <+> kw "=" <+> renderJson v
  AesonPatch.Tst p v ->
    kw "test" <+> ppDiffPointer p <+> kw "==" <+> renderJson v

ppPatch :: Patch -> Doc AnsiStyle
ppPatch = vsep . fmap ppDiffOperation . patchOperations

infix 4 `yamlShouldBe`
yamlShouldBe :: HasCallStack => ByteString -> Aeson.Value -> Assertion
yamlShouldBe actualBs expected =
  Yaml.decodeThrow actualBs >>= \actual ->
    case AesonDiff.diff actual expected of
      AesonDiff.Patch diffs | null diffs -> pure ()
      patch ->
        assertFailureDoc . vsep $
          [ errDoc "expected YAML to be the same, but actual:"
          , indent 2 (renderYaml expected)
          , errDoc "differs from expected:"
          , indent 2 (ppPatch patch)
          ]

linear :: TestTree
linear =
  testGroup
    "linear"
    [ testCase
        "load"
        ( case loadFactory (Yaml.encode linearYaml) of
            Left err -> assertFailureDoc . ppLoadError $ err
            Right (warnings, factSt) -> do
              assertBoolDoc
                ( vsep
                    [ errDoc "warnings during load:"
                    , indent 2 . vsep . fmap ppLoadWarning $ warnings
                    , errDoc "but loaded:"
                    , indent 2 . ppFactorySt $ factSt
                    ]
                )
                (null warnings)
              factSt `factoryStShouldBe` linearSt
        )
    , testCase "store" (storeFactory linearSt `yamlShouldBe` linearYaml)
    ]

linearYaml :: Aeson.Value
linearYaml =
  [aesonQQ|
  {
    "belts": [{"downstream": 2, "upstream": 1, "item": "testB"}],
    "clusters": [
      {"node": 1, "quantity": 1, "recipeKey": {"machine": "test", "identifier": "a1b1"}},
      {"node": 2, "quantity": 1, "recipeKey": {"machine": "test", "identifier": "b1c1"}}
    ],
    "customRecipes": [
      {
        "key": {"machine": "test", "identifier": "a1b1"},
        "cycleTime": 1,
        "transfer": {"inputs": {"testA": 1}, "outputs": {"testB": 1}}
      },
      {
        "key": {"machine": "test", "identifier": "b1c1"},
        "cycleTime": 1,
        "transfer": {"inputs": {"testB": 1}, "outputs": {"testC": 1}}
      }
    ]
  }
  |]

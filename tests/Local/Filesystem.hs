
module Local.Filesystem where

import           Data.Aeson
import           Data.Monoid
import           Test.Tasty             (TestTree)
import qualified Test.Tasty.HUnit       as HU
import           Data.Text              (Text)

import           Data.JsonSchema.Draft4

fetchFromFilesystem :: [TestTree]
fetchFromFilesystem =
  [ HU.testCase
      "readFile exceptions are turned into Lefts"
      readFileExceptions

  , HU.testCase
      "Relative reference starting with ./"
      (resolve "./tests/Local/schema.json")
  , HU.testCase
      "Relative reference"
      (resolve "tests/Local/schema.json")
  , HU.testCase
      "Chained relative references starting with ./"
      (resolve "./tests/Local/schema-with-ref-1.json")
  , HU.testCase
      "Chained relative references where the second starts with ./"
      (resolve "tests/Local/schema-with-ref-1.json")
  , HU.testCase
      "Chained relative references where the first starts with ./"
      (resolve "./tests/Local/schema-with-ref-2.json")
  , HU.testCase
      "Chained relative references"
      (resolve "tests/Local/schema-with-ref-2.json")
  ]

readFileExceptions :: IO ()
readFileExceptions = do
  res <- referencesViaFilesystem (SchemaWithURI schema Nothing)
  case res of
    Left (ReadFailure _) -> pure ()
    a                    -> error (msg <> show a)
  where
    schema :: Schema
    schema = emptySchema { _schemaRef = Just "does-not-exist.json" }

    msg :: String
    msg = "expected referencesViaFilesystem to return ReadFailure,"
       <> " instead got: "

-- * Helpers

resolve :: Text -> IO ()
resolve ref = do
  res <- fetchFilesystemAndValidate (SchemaWithURI schema Nothing) badData
  case res of
    Left (FVData [_]) -> pure ()
    a                 -> error (msg <> show a)
  where
    schema :: Schema
    schema = emptySchema { _schemaRef = Just ref }

    badData :: Value
    badData = toJSON [True, True]

    msg :: String
    msg = "expected fetchFilesystemAndValidate to return"
       <> " Left (FVData [_]), instead got: "

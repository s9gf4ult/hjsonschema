
module Local.Filesystem where

import           Data.Aeson
import           Data.Monoid
import           Test.Tasty             (TestTree)
import qualified Test.Tasty.HUnit       as HU

import           Data.JsonSchema.Draft4

fetchFromFilesystem :: [TestTree]
fetchFromFilesystem =
  [ HU.testCase "readFile exceptions are turned into Lefts" readFileExceptions
  , HU.testCase "Relative references are parsed correctly" relativeReferences
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

relativeReferences :: IO ()
relativeReferences = do
  res <- fetchFilesystemAndValidate (SchemaWithURI schema Nothing) badData
  case res of
    Left (FVData [Failure (Ref UniqueItems) _ _]) -> pure ()
    a                                             -> error (msg <> show a)
  where
    schema :: Schema
    schema = emptySchema { _schemaRef = Just "tests/Local/schema.json" }

    badData :: Value
    badData = toJSON [True, True]

    msg :: String
    msg = "expected fetchFilesystemAndValidate to return"
       <> " Right (Right [Failure (Ref UniqueItems) _ _]), instead got: "

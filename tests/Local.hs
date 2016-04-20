
module Main where

import           Control.Applicative
import           Control.Monad          (unless)
import           Data.Aeson
import qualified Data.Aeson.Pointer     as P
import           Data.List              (isSuffixOf)
import           Data.Monoid
import           System.Directory       (getDirectoryContents)
import           Test.Tasty             (TestTree, defaultMain, testGroup)
import qualified Test.Tasty.HUnit       as HU
import           Test.Tasty.QuickCheck  (testProperty)

import           Data.JsonSchema.Draft4
import qualified Data.Validator.Draft4  as VA
import           Shared                 (isLocal, readSchemaTests, toTest)

dir :: String
dir = "JSON-Schema-Test-Suite/tests/draft4"

main :: IO ()
main = do
  filenames <- filter isLocal . filter (".json" `isSuffixOf`) <$> getDirectoryContents dir
  ts <- readSchemaTests dir filenames
  defaultMain . testGroup "Tests not requiring an HTTP server" $
      testProperty "Invert schemas through JSON without change" invertSchema
    : testGroup "Make paths to invalid data correctly" correctPaths
    : testGroup "Test the referencesViaFilesystem function" fetchFromFilesystem
    : fmap toTest ts

invertSchema :: Schema -> Bool
invertSchema a = Just a == decode (encode a)

correctPaths :: [TestTree]
correctPaths =
  [ HU.testCase "Items object" itemsObject
  , HU.testCase "Items array" itemsArray
  ]

itemsObject :: IO ()
itemsObject = HU.assertEqual "Path to invalid data"
                             (P.Pointer [P.Token "0"])
                             (_failureOffendingData failure)
  where
    [failure] = runValidate (ReferencedSchemas schema mempty)
                            sw (toJSON [[True, True]])

    schema :: Schema
    schema = emptySchema
      { _schemaItems = Just (VA.ItemsObject (emptySchema { _schemaUniqueItems = Just True }))
      }

    sw :: SchemaWithURI Schema
    sw = SchemaWithURI
      { _swSchema = schema
      , _swURI    = Nothing
      }

itemsArray :: IO ()
itemsArray = HU.assertEqual "Path to invalid data"
                            (P.Pointer [P.Token "0"])
                            (_failureOffendingData failure)
  where
    [failure] = runValidate (ReferencedSchemas schema mempty)
                            sw (toJSON [[True, True]])

    schema :: Schema
    schema = emptySchema
      { _schemaItems = Just (VA.ItemsArray [emptySchema { _schemaUniqueItems = Just True }])
      }

    sw :: SchemaWithURI Schema
    sw = SchemaWithURI
      { _swSchema = schema
      , _swURI    = Nothing
      }

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
    schema = emptySchema { _schemaRef = Just "tests/local-schema.json" }

    badData :: Value
    badData = toJSON [True, True]

    msg :: String
    msg = "expected fetchFilesystemAndValidate to return"
       <> " Right (Right [Failure (Ref UniqueItems) _ _]), instead got: "

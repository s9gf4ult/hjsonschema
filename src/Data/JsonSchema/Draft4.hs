{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.JsonSchema.Draft4
  ( Schema(..)
  , emptySchema
  , checkSchema

    -- * One-step validation
  , FilesystemValidateFailure(..)
  , fetchFilesystemAndValidate

    -- * Fetching tools
  , SchemaWithURI(..)
  , ReferencedSchemas(..)
  , HTTPFailure(..)
  , referencesViaHTTP
  , FilesystemFailure(..)
  , referencesViaFilesystem

    -- * Failure
  , Failure(..)
  , ValidatorChain(..)

    -- * Other Draft 4 things exported just in case
  , draft4Spec
  , schemaValidity
  , IN.runValidate
  ) where

import           Control.Arrow                   (left)
import           Data.Aeson
import qualified Data.ByteString.Lazy            as LBS
import           Data.FileEmbed
import qualified Data.HashMap.Strict             as H
import           Data.Maybe                      (fromMaybe)

import           Data.JsonSchema.Draft4.Failure
import qualified Data.JsonSchema.Draft4.Internal as IN
import           Data.JsonSchema.Draft4.Schema
import           Data.JsonSchema.Fetch           (FilesystemFailure(..),
                                                  HTTPFailure(..),
                                                  ReferencedSchemas(..),
                                                  SchemaWithURI(..))
import qualified Data.JsonSchema.Fetch           as FE

data FilesystemValidateFailure
  = FVFilesystem FilesystemFailure
  | FVSchema     [Failure]
  | FVData       [Failure]
  deriving Show

fetchFilesystemAndValidate
  :: SchemaWithURI Schema
  -> Value
  -> IO (Either FilesystemValidateFailure ())
fetchFilesystemAndValidate sw v = do
  res <- referencesViaFilesystem sw
  pure (g =<< f =<< left FVFilesystem res)
  where
    f :: ReferencedSchemas Schema
      -> Either FilesystemValidateFailure (Value -> [Failure])
    f references = left FVSchema (checkSchema references sw)

    g :: (Value -> [Failure]) -> Either FilesystemValidateFailure ()
    g validate = case validate v of
                   []       -> Right ()
                   failures -> Left (FVData failures)

-- | Check the validity of a schema and return a function to validate data.
checkSchema
  :: ReferencedSchemas Schema
  -> SchemaWithURI Schema
  -> Either [Failure] (Value -> [Failure])
checkSchema referenced schemaWithURI =
  case schemaValidity (_swSchema schemaWithURI) of
    []       -> Right (IN.runValidate referenced schemaWithURI)
    failures -> Left failures

draft4Spec :: FE.Spec Schema
draft4Spec = FE.Spec IN.embedded _schemaId _schemaRef

referencesViaHTTP
  :: SchemaWithURI Schema
  -> IO (Either HTTPFailure (ReferencedSchemas Schema))
referencesViaHTTP = FE.referencesViaHTTP' draft4Spec

referencesViaFilesystem
  :: SchemaWithURI Schema
  -> IO (Either FilesystemFailure (ReferencedSchemas Schema))
referencesViaFilesystem = FE.referencesViaFilesystem' draft4Spec

-- | In normal situations just use 'checkSchema', which is a combination of
-- 'schemaValidity' and 'runValidate'.
schemaValidity :: Schema -> [Failure]
schemaValidity = IN.runValidate referenced (SchemaWithURI d4 Nothing) . toJSON
  where
    d4 :: Schema
    d4 = fromMaybe (error "Schema decode failed (this should never happen)")
       . decode . LBS.fromStrict $ $(embedFile "src/draft4.json")

    referenced :: ReferencedSchemas Schema
    referenced = ReferencedSchemas
                   d4
                   (H.singleton "http://json-schema.org/draft-04/schema" d4)

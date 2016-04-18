{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.JsonSchema.Draft4
  ( Schema(..)
  , emptySchema
  , checkSchema

    -- * Fetching tools
  , SchemaWithURI(..)
  , ReferencedSchemas(..)
  , fetchReferencedSchemas

    -- * Failure
  , Failure(..)
  , ValidatorChain(..)

    -- * Other Draft 4 things exported just in case
  , schemaValidity
  , IN.runValidate
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy            as LBS
import           Data.FileEmbed
import qualified Data.HashMap.Strict             as H
import           Data.Maybe                      (fromMaybe)

import           Data.JsonSchema.Draft4.Failure
import qualified Data.JsonSchema.Draft4.Internal as IN
import           Data.JsonSchema.Draft4.Schema
import           Data.JsonSchema.Fetch           (ReferencedSchemas(..),
                                                  SchemaWithURI(..), Spec(..))
import qualified Data.JsonSchema.Fetch           as FE
import           Import

-- | Check the validity of a schema and return a function to validate data.
checkSchema
  :: ReferencedSchemas Schema
  -> SchemaWithURI Schema
  -> Either [Failure] (Value -> [Failure])
checkSchema referenced schemaWithURI =
  case schemaValidity (_swSchema schemaWithURI) of
    []       -> Right (IN.runValidate referenced schemaWithURI)
    failures -> Left failures

fetchReferencedSchemas
  :: SchemaWithURI Schema
  -> IO (Either Text (ReferencedSchemas Schema))
fetchReferencedSchemas =
  FE.fetchReferencedSchemas (Spec IN.embedded _schemaId _schemaRef)

-- | In normal situations just use 'checkSchema', which is a combination of
-- 'schemaValidity' and 'runValidate'.
schemaValidity :: Schema -> [Failure]
schemaValidity = IN.runValidate referenced (SchemaWithURI d4 Nothing) . toJSON
  where
    d4 :: Schema
    d4 = fromMaybe (error "Schema decode failed (this should never happen)")
       . decode . LBS.fromStrict $ $(embedFile "draft4.json")

    referenced :: ReferencedSchemas Schema
    referenced = ReferencedSchemas
                   d4
                   (H.singleton "http://json-schema.org/draft-04/schema" d4)

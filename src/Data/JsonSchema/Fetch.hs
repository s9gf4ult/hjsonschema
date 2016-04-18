{-# LANGUAGE ScopedTypeVariables #-}

module Data.JsonSchema.Fetch where

import           Control.Exception        (SomeException(..), catch)
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.HashMap.Strict      as H
import qualified Data.Text                as T
import           Network.HTTP.Client

import           Data.Validator.Reference (isRemoteReference,
                                           newResolutionScope,
                                           resolveReference)
import           Import

-- For GHCs before 7.10:
import           Prelude                  hiding (concat, sequence)

data Spec schema = Spec
  { _ssEmbedded :: schema -> [schema]
  , _ssGetId    :: schema -> Maybe Text
  , _ssGetRef   :: schema -> Maybe Text
  }

data SchemaWithURI schema = SchemaWithURI
  { _swSchema :: !schema
  , _swURI    :: !(Maybe Text)
  -- ^ Must not include a URI fragment, e.g. use
  -- "http://example.com/foo" not "http://example.com/foo#bar".
  --
  -- This is the URI identifying the document containing the schema.
  -- It's different than the schema's "id" field, which controls scope
  -- when resolving references contained in the schema.

  -- TODO: Make the no URI fragment requirement unnecessary.
  } deriving (Eq, Show)

-- | Keys are URIs (without URI fragments).
type URISchemaMap schema = HashMap Text schema

data ReferencedSchemas schema = ReferencedSchemas
  { _rsStarting  :: !schema
  -- ^ Used to resolve relative references.
  , _rsSchemaMap :: !(URISchemaMap schema)
  } deriving (Eq, Show)

-- | Take a schema. Retrieve every document either it or its subschemas
-- include via the "$ref" keyword.
fetchReferencedSchemas
  :: forall schema. FromJSON schema
  => Spec schema
  -> SchemaWithURI schema
  -> IO (Either Text (ReferencedSchemas schema))
fetchReferencedSchemas spec sw = do
  manager <- newManager defaultManagerSettings
  let f = fetchReferencedSchemas' spec (simpleGET manager) sw
  catch (Right <$> f) handler
  where
    handler :: SomeException -> IO (Either Text (ReferencedSchemas schema))
    handler e = pure . Left . T.pack . show $ e

-- | Based on 'Network.Http.Conduit.simpleHttp' from http-conduit.
simpleGET :: Manager -> Text -> IO LBS.ByteString
simpleGET man url = do
  req <- parseUrl (T.unpack url)
  responseBody <$> httpLbs req { requestHeaders = ("Connection", "close")
                               : requestHeaders req
                               } man

fetchReferencedSchemas'
  :: forall schema. FromJSON schema
  => Spec schema
  -> (Text -> IO LBS.ByteString)
  -> SchemaWithURI schema
  -> IO (ReferencedSchemas schema)
fetchReferencedSchemas' spec fetchRef sw =
  ReferencedSchemas (_swSchema sw) <$> foo spec fetchRef mempty sw

-- | A version of 'fetchReferencedSchema's where the function to fetch
-- schemas is provided by the user. This allows restrictions to be added,
-- e.g. rejecting non-local URIs.
foo
  :: forall schema. FromJSON schema
  => Spec schema
  -> (Text -> IO LBS.ByteString)
  -> URISchemaMap schema
  -> SchemaWithURI schema
  -> IO (URISchemaMap schema)
foo spec@(Spec embedded getId getRef) fetchRef referenced sw =
  foldlM fetchRecursively referenced (includeSubschemas spec sw)
  where
    fetchRecursively
      :: URISchemaMap schema
      -> SchemaWithURI schema
      -> IO (URISchemaMap schema)
    fetchRecursively g (SchemaWithURI schema mUri) = do
      -- Resolving the new scope is necessary here because of situations
      -- like this:
      --
      -- {
      --     "id": "http://localhost:1234/",
      --     "items": {
      --         "id": "folder/",
      --         "items": {"$ref": "folderInteger.json"}
      --     }
      -- }
      let scope = newResolutionScope mUri (getId schema)
      case resolveReference scope <$> getRef schema of
        Just (Just uri,_) ->
          if not (isRemoteReference uri) || H.member uri g
            then pure g
            else do
              bts <- fetchRef uri
              case eitherDecode bts of
                Left e     -> ioError (userError e)
                Right schm ->
                  foo spec fetchRef (H.insert uri schm g)
                      (SchemaWithURI schm (Just uri))
        _ -> pure g

-- | Return the schema passed in as an argument, as well as every
-- subschema contained within it.
includeSubschemas
  :: forall schema.
     Spec schema
  -> SchemaWithURI schema
  -> [SchemaWithURI schema]
includeSubschemas spec@(Spec embedded getId _) (SchemaWithURI schema mUri) =
  SchemaWithURI schema mUri
  : (includeSubschemas spec =<< subSchemas)
  where
    subSchemas :: [SchemaWithURI schema]
    subSchemas = flip SchemaWithURI (newResolutionScope mUri (getId schema))
             <$> embedded schema

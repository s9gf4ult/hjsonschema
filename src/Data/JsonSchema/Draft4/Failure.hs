
module Data.JsonSchema.Draft4.Failure where

import           Data.Text (Text)
import qualified Data.Validator.Failure as FR

type Invalid = FR.Failure ValidatorChain

data ValidatorChain
  = MultipleOf
  | Maximum
  | ExclusiveMaximum
  | Minimum
  | ExclusiveMinimum

  | MaxLength
  | MinLength
  | PatternValidator

  | MaxItems
  | MinItems
  | UniqueItems
  | Items ValidatorChain
  | AdditionalItemsBool
  | AdditionalItemsObject ValidatorChain

  | MaxProperties
  | MinProperties
  | Required
  | SchemaDependency ValidatorChain
  | PropertyDependency
  | Properties ValidatorChain
  | PatternProperties ValidatorChain
  | AdditionalPropertiesBool
  | AdditionalPropertiesObject ValidatorChain

  | RefResolution
    -- ^ Indicates a reference that failed to resolve.
    --
    -- NOTE: The language agnostic test suite doesn't specify if this should
    -- cause a validation error or should allow data to pass. We choose to
    -- return a validation error.
    --
    -- Also note that ideally we would enforce in the type system that any
    -- failing references be dealt with before valididation. Then this could
    -- be removed entirely.
  | Ref Text ValidatorChain
  | Enum
  | TypeValidator
  | AllOf ValidatorChain
  | AnyOf
  | OneOf
  | NotValidator
  deriving (Eq, Show)

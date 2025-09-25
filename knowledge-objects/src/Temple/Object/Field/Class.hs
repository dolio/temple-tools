
module Temple.Object.Field.Class where

import Data.String
import Text.Megaparsec

import Temple.Object.Field.Type

class (Bounded f, Enum f) => Field f where
  fieldName :: IsString s => f -> s
  fieldType :: f -> FieldType
  isPadding :: f -> Bool

parseFieldName :: Field f => MonadParsec e s m => IsString (Tokens s) => m f
parseFieldName = choice $ token <$> [minBound .. maxBound] where
  token f = f <$ try (chunk $ fieldName f)

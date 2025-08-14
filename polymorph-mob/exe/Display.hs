
module Display (displayMob) where

import Data.ByteString.Builder
import Data.Map.Strict (Map, toList)
import Data.UUID

import Temple.Objects.Spec

import Mob

displayMob :: Mob -> Builder
displayMob (Mob { .. })
  = mconcat
  [ byteString "{ \"proto-id\": "
    , string8 $ show protoId
    , char8 '\n'
  , byteString ", \"uuid\": \""
    , byteString $ toASCIIBytes uuid
    , byteString "\"\n"
  , byteString ", \"object-type\": \""
    , string8 $ typeName objType
    , byteString "\"\n"
  ]
  <> displayFields fields
  <> byteString "}\n"

displayFields :: Map ObjectField Value -> Builder
displayFields = foldMap (uncurry displayField) . toList

displayField :: ObjectField -> Value -> Builder
displayField fld val
  = mconcat
  [ byteString ", \""
  , string8 $ fieldName fld
  , byteString "\": "
  , displayValue val
  , byteString "\n"
  ]

displayValue :: Value -> Builder
displayValue = \case
  W32 w -> string8 $ show w
  W64 w -> string8 $ show w
  I32 i -> string8 $ show i
  F32 f -> string8 $ show f
  B32 b -> string8 $ show b
  UID u -> byteString "\"" <> byteString (toASCIIBytes u) <> byteString "\""
  W32x2 w0 w1 ->
    mconcat
      [ byteString "["
      , string8 $ show w0
      , byteString ","
      , string8 $ show w1
      , byteString "]"
      ]



module Temple.Object.Field.Key where

import Data.String
import Text.Megaparsec

import Temple.Object.Field.Type

data KeyField
  = KeyBegin
  | KeyKeyId
  | KeyPadInt1
  | KeyPadInt2
  | KeyPadIntArr1
  | KeyPadInt64Arr1
  | KeyEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum KeyField where
  fromEnum = \case
    KeyBegin        -> 255
    KeyKeyId        -> 256
    KeyPadInt1      -> 257
    KeyPadInt2      -> 258
    KeyPadIntArr1   -> 259
    KeyPadInt64Arr1 -> 260
    KeyEnd          -> 261

  toEnum = \case
    255 -> KeyBegin
    256 -> KeyKeyId
    257 -> KeyPadInt1
    258 -> KeyPadInt2
    259 -> KeyPadIntArr1
    260 -> KeyPadInt64Arr1
    261 -> KeyEnd
    n -> error $ "toEnum @KeyField: bad value: " ++ show n

  enumFrom n = enumFromTo n maxBound
  enumFromThen m n = enumFromThenTo m n maxBound

keyFieldName :: KeyField -> String
keyFieldName = \case
  KeyBegin        -> "obj_f_key_begin"
  KeyKeyId        -> "obj_f_key_key_id"
  KeyPadInt1      -> "obj_f_key_pad_i_1"
  KeyPadInt2      -> "obj_f_key_pad_i_2"
  KeyPadIntArr1   -> "obj_f_key_pad_ias_1"
  KeyPadInt64Arr1 -> "obj_f_key_pad_i64as_1"
  KeyEnd          -> "obj_f_key_end"

keyFieldType :: KeyField -> FieldType
keyFieldType = \case
  KeyBegin -> BeginF
  KeyKeyId -> W32F
  KeyPadInt1 -> W32F
  KeyPadInt2 -> W32F
  KeyPadIntArr1 -> W32ArrF
  KeyPadInt64Arr1 -> W64ArrF
  KeyEnd -> EndF

parsePartialKeyFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m KeyField
parsePartialKeyFieldName =
  choice
    [ KeyBegin        <$ chunk "begin"
    , KeyKeyId        <$ chunk "key_id"
    , KeyPadInt1      <$ chunk "pad_i_1"
    , KeyPadInt2      <$ chunk "pad_i_2"
    , KeyPadIntArr1   <$ chunk "pad_ias_1"
    , KeyPadInt64Arr1 <$ chunk "pad_i64as_1"
    , KeyEnd          <$ chunk "end"
    ]

parseKeyFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m KeyField
parseKeyFieldName = chunk "obj_f_key_" *> parsePartialKeyFieldName

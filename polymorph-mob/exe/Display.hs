
module Display (displayMob) where

import Data.ByteString.Builder
import Data.Char (toLower)
import Data.Int
import Data.List (intersperse)
import Data.Map.Strict (Map, toList)
import Data.UUID
import Data.Word

import Temple.Objects.Spec

import Mob

displayMob :: Mob -> Builder
displayMob (Mob { .. })
  = mconcat
  [ byteString "{ \"proto-id\": "
    , string8 $ show protoId
    , char8 '\n'
  , byteString ", \"vuuid\": "
    , displayVUUID vuuid
    , byteString "\"\n"
  , byteString ", \"object-type\": \""
    , string8 $ typeName objType
    , byteString "\"\n"
  ]
  <> displayFields fields
  <> byteString "}\n"

displayVUUID :: VUUID -> Builder
displayVUUID (VUUID {..})
  = mconcat
  [ byteString "{ \"variant\": "
  , string8 (show variant)
  , byteString ", \"uuid\": "
  , displayUUID uuid
  , byteString " }"
  ]

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
  B32 b -> string8 . fmap toLower $ show b
  UID u -> displayVUUID u
  Loc x y -> displayLoc x y
  I32Arr is -> displayArr (string8 . show) is
  W32Arr ws -> displayArr (string8 . show) ws
  W64Arr ws -> displayArr (string8 . show) ws
  LocArr ls -> displayArr (uncurry displayLoc) ls
  ObjArr us -> displayArr displayVUUID us
  ScriptArr ss -> displayArr displayScript ss

displayLoc :: Int32 -> Int32 -> Builder
displayLoc x y =
  mconcat
    [ byteString "{ \"locx\": "
    , string8 $ show x
    , byteString ", \"locy\": "
    , string8 $ show y
    , byteString " }"
    ]

displayUUID :: UUID -> Builder
displayUUID u =
  byteString "\"" <> byteString (toASCIIBytes u) <> byteString "\""

displayArr :: (a -> Builder) -> [a] -> Builder
displayArr de es =
  mconcat
    [ byteString "["
    , mconcat . intersperse (char8 ',') $ fmap de es
    , byteString "]"
    ]

displayScript :: (Word32, Word32, Word32) -> Builder
displayScript (unk, count, id) =
  mconcat
    [ byteString "{ \"unknown\": "
    , string8 $ show unk
    , byteString ", \"counters\": "
    , string8 $ show count
    , byteString ", \"script-id\": "
    , string8 $ show id
    , byteString " }"
    ]

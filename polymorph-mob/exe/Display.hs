
module Display (displayMob) where

import Data.ByteString.Builder
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Map.Strict (Map, toList)
import Data.UUID
import Data.Word

import Numeric (showHex)

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
  Loc l -> displayLoc l
  I32Arr is -> displayArr (string8 . show) is
  W32Arr ws -> displayArr (string8 . show) ws
  W64Arr ws -> displayArr (string8 . show) ws
  ObjArr us -> displayArr displayVUUID us
  ScriptArr ss -> displayArr displayScript ss
  StandptArr sps -> displayArr displayStandpoint sps
  WayptArr (Waypts count ex1 ex2 ex3 wps) ->
    mconcat
      [ "{ \"count\": "
      , string8 $ show count
      , ", \"extra1\": "
      , string8 $ show ex1
      , ", \"extra2\": "
      , string8 $ show ex2
      , ", \"extra3\": "
      , string8 $ show ex3
      , ", \"waypoints\": "
      , displayArr displayWaypoint wps
      ]

displayLoc :: Loc -> Builder
displayLoc (L {..}) =
  mconcat
    [ byteString "{ \"locx\": "
    , string8 $ show x
    , byteString ", \"locy\": "
    , string8 $ show y
    , byteString " }"
    ]

displayOffsets :: Offsets -> Builder
displayOffsets (Off {..}) =
  mconcat
    [ byteString "{ \"offx\": "
    , string8 $ show offx
    , byteString ", \"offy\": "
    , string8 $ show offy
    , byteString " }"
    ]

displayStandpoint :: Standpoint -> Builder
displayStandpoint (Stdpt {..}) =
  mconcat
    [ byteString "{ \"map-info\": "
    , string8 $ show mapInfo
    , byteString ", \"loc\": "
    , displayLoc loc
    , byteString ", \"offsets\": "
    , displayOffsets offsets
    , byteString ", \"jp\": "
    , string8 $ show jp
    , byteString " }"
    ]

displayWaypoint :: Waypoint -> Builder
displayWaypoint (Waypt {..}) =
  mconcat
    [ byteString "{ \"flags\": "
    , string8 $ show wayptFlags
    , byteString ", \"loc\": "
    , displayLoc wayptLoc
    , byteString ", \"offsets\": "
    , displayOffsets wayptOffs
    , byteString ", \"rotation\": "
    , string8 $ show wayptRot
    , byteString ", \"anims\": 0x"
    , string8 $ showHex wayptAnims ""
    , byteString ", \"delay\": "
    , string8 $ show wayptDelay
    , byteString ", \"extra\": "
    , displayArr (string8 . show) wayptExtra
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


module Display (displayMob) where

import Data.ByteString.Builder
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Map.Strict (Map, toList)
import Data.UUID

import Numeric (showHex)

import Temple.Objects.Spec

import Bitmap
import Mob

displayMob :: Mob -> Builder
displayMob (Mob {..})
  = mconcat
  [ byteString "{ \"object-info\":\n"
    , displayObjectInfo objInfo
    , char8 '\n'
  , byteString ", \"object-id\": "
    , displayObjectId True objId
    , byteString "\n"
  , byteString ", \"object-type\": \""
    , string8 $ typeName objType
    , byteString "\"\n"
  ]
  <> displayFields fields
  <> byteString "}\n"

displayObjectInfo :: ObjectInfo -> Builder
displayObjectInfo (ObjInfo {..})
  = mconcat
  [ byteString "    { \"subtype\": "
  , string8 $ show subtype
  , byteString "\n    , \"compat\": 0x"
  , string8 $ showHex compat ""
  , byteString "\n    , \"unknown2\": 0x"
  , string8 $ showHex oiUnknown2 ""
  , byteString "\n    , \"protoId\": "
  , string8 $ show protoId
  , byteString "\n    , \"unknown3\": 0x"
  , string8 $ showHex oiUnknown3 ""
  , byteString "\n    , \"unknown4\": 0x"
  , string8 $ showHex oiUnknown4 ""
  , byteString "\n    , \"unknown5\": 0x"
  , string8 $ showHex oiUnknown5 ""
  , byteString "\n    }"
  ]

displayObjectId :: Bool -> ObjectId -> Builder
displayObjectId newline (ObjId {..})
  = mconcat
  [ break
  , byteString "{ \"variant\": "
  , string8 (show variant)
  , break
  , byteString ", \"uuid\": "
  , displayUUID uuid
  , break
  , char8 '}'
  ]
  where
  break | newline = byteString "\n    " | otherwise = char8 ' '

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
  Null -> byteString "null"
  W32 w -> string8 $ show w
  W64 w -> string8 $ show w
  I32 i -> string8 $ show i
  F32 f -> string8 $ show f
  B32 b -> string8 . fmap toLower $ show b
  Obj u -> displayObjectId False u
  Loc l -> displayLoc l
  I32Arr is -> displayArray (string8 . show) is
  W32Arr ws -> displayArray (string8 . show) ws
  W64Arr ws -> displayArray (string8 . show) ws
  ObjArr us -> displayArray (displayObjectId False) us
  ScriptArr ss -> displayScriptArray ss
  StandptArr sps -> displayArray displayStandpoint sps
  String s -> char8 '"' <> byteString s <> char8 '"'
  WayptArr (Waypts {..}) ->
    mconcat
      [ "{ \"count\": "
      , string8 $ show wayptCount
      , ", \"extra1\": "
      , string8 $ show wayptExtra1
      , ", \"extra2\": "
      , string8 $ show wayptExtra2
      , ", \"extra3\": "
      , string8 $ show wayptExtra3
      , ", \"waypoints\": "
      , displays displayWaypoint waypts
      , " }"
      ]

displayArray :: (e -> Builder) -> Array e -> Builder
displayArray de (Dense {..}) = displays de content
displayArray de (Sparse {..}) =
  mconcat
    [ byteString "{ \"elems\": "
    , displays de content
    , byteString ", \"bitmap\": "
    , displayBitmap _bitmap
    , byteString " }"
    ]

displayLoc :: Loc -> Builder
displayLoc (L {..}) =
  mconcat
    [ byteString "{ \"locx\": "
    , string8 $ show locx
    , byteString ", \"locy\": "
    , string8 $ show locy
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
    , displays (string8 . show) wayptExtra
    , byteString " }"
    ]

displayUUID :: UUID -> Builder
displayUUID u =
  byteString "\"" <> byteString (toASCIIBytes u) <> byteString "\""

displays :: (a -> Builder) -> [a] -> Builder
displays de es =
  mconcat
    [ byteString "["
    , mconcat . intersperse (char8 ',') $ fmap de es
    , byteString "]"
    ]

displayScript :: Script -> Builder
displayScript (Script {..}) =
  mconcat
    [ byteString "{ \"unknown\": "
    , string8 $ show scrUnknown
    , byteString ", \"counters\": "
    , string8 $ show scrCounters
    , byteString ", \"script-id\": "
    , string8 $ show scrId
    , byteString " }"
    ]

displayScriptArray :: Map ObjectScript Script -> Builder
displayScriptArray ss
  = byteString "\n    { "
 <> intercalate comma (f <$> toList ss)
 <> byteString "\n    }"
 where
 comma = byteString "\n    , "
 f :: (ObjectScript, Script) -> Builder
 f (scn, scr) =
   mconcat
     [ char8 '"'
     , string8 (objectScriptName scn)
     , byteString "\": "
     , displayScript scr
     ]

displayBitmap :: Bitmap -> Builder
displayBitmap bs = char8 '"' <> displayBits bs <> char8 '"'

intercalate :: Monoid m => m -> [m] -> m
intercalate e = mconcat . intersperse e

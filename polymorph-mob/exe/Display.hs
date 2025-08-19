
module Display (displayMob) where

import Data.ByteString.Builder
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Map.Strict (Map, toList)
import Data.UUID

import Numeric (showHex)

import Temple.Object.Field
import Temple.Object.Script
import Temple.Object.Type

import Bitmap
import Mob

displayMob :: Mob -> Builder
displayMob (Mob {..})
  = mconcat
  [ byteString "{ \"mob-info\":\n"
    , displayObjectInfo objInfo
    , char8 '\n'
  , byteString ", \"mob-id\": "
    , displayObjectId True objId
    , byteString "\n"
  , byteString ", \"mob-type\": \""
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
  I32Arr is -> displayArray Nothing (string8 . show) is
  W32Arr ws -> displayArray Nothing (string8 . show) ws
  W64Arr ws -> displayArray Nothing (string8 . show) ws
  ObjArr us -> displayArray (Just 4) (displayObjectId False) us
  ScriptArr ss -> displayScriptArray ss
  StandptArr sps -> displayArray (Just 4) displayStandpoint sps
  String s -> char8 '"' <> byteString s <> char8 '"'
  WayptArr (Waypts {..}) ->
    mconcat
      [ brk <> "{ \"count\": "
      , string8 $ show wayptCount
      , brk <> ", \"extra1\": "
      , string8 $ show wayptExtra1
      , brk <> ", \"extra2\": "
      , string8 $ show wayptExtra2
      , brk <> ", \"extra3\": "
      , string8 $ show wayptExtra3
      , brk <> ", \"waypoints\": "
      , displays (Just 8) displayWaypoint waypts
      , brk <> "}"
      ]
    where brk = indent $ Just 4

displayArray :: Maybe Int -> (e -> Builder) -> Array e -> Builder
displayArray ind de (Dense {..}) = displays ind de content
displayArray ind de (Sparse {..}) =
  mconcat
    [ byteString "{ \"elems\": "
    , displays ind de content
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
    , brk <> byteString ", \"loc\": "
    , displayLoc loc
    , brk <> byteString ", \"offsets\": "
    , displayOffsets offsets
    , brk <> byteString ", \"jp\": "
    , string8 $ show jp
    , brk <> byteString "}"
    ]
  where brk = indent $ Just 6

displayWaypoint :: Waypoint -> Builder
displayWaypoint (Waypt {..}) =
  mconcat
    [ byteString "{ \"flags\": "
    , string8 $ show wayptFlags
    , brk <> byteString ", \"loc\": "
    , displayLoc wayptLoc
    , brk <> byteString ", \"offsets\": "
    , displayOffsets wayptOffs
    , brk <> byteString ", \"rotation\": "
    , string8 $ show wayptRot
    , brk <> byteString ", \"anims\": 0x"
    , string8 $ showHex wayptAnims ""
    , brk <> byteString ", \"delay\": "
    , string8 $ show wayptDelay
    , brk <> byteString "}"
    ]
  where brk = indent $ Just 10

displayUUID :: UUID -> Builder
displayUUID u =
  byteString "\"" <> byteString (toASCIIBytes u) <> byteString "\""

displays :: Maybe Int -> (a -> Builder) -> [a] -> Builder
displays ind de es =
  mconcat
    [ brk <> byteString "[" <> isp
    , intercalate (brk <> byteString ", ") $ fmap de es
    , brk <> byteString "]"
    ]
  where
  brk = indent ind
  isp = maybe mempty (const $ char8 ' ') ind

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

indent :: Maybe Int -> Builder
indent = \case
  Nothing -> mempty
  Just ind -> mconcat $ char8 '\n' : replicate ind (char8 ' ')


module Display
  ( displayMob
  , displayDiff
  , displayPlayer
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.UUID
import Data.Word

import Numeric (showHex)

import Temple.Object.Field
import Temple.Object.Script
import Temple.Object.Type

import Bitmap
import Mob

displayMob :: Map Word32 ByteString -> Mob -> Builder
displayMob condNames (Mob {..})
  = mconcat
  [ byteString "{ \"mob-info\":\n"
    , displayObjectInfo objInfo
    , char8 '\n'
  , byteString ", \"mob-id\": "
    , displayObjectId True objId
    , byteString "\n"
  , byteString ", \"mob-type\": \""
    , string8 $ typeName objType
    , byteString "\"\n, "
  , displayFields condNames fields
  , byteString "\n}\n"
  ]

displayDiff :: Map Word32 ByteString -> MobDiff -> Builder
displayDiff condNames (MobDiff {..})
  = mconcat
  [ byteString "{ "
  , displayFields condNames diffFields
  , byteString "\n}\n"
  ]

displayPlayer :: Map Word32 ByteString -> Player -> Builder
displayPlayer condNames (Player { pcData = Mob {..}, ..})
  = mconcat
  [ byteString "{ \"pc-flags\": 0x"
    , string8 $ showHex pcFlags ""
    , char8 '\n'
  , byteString ", \"pc-id\": "
    , displayObjectId True pcId
    , char8 '\n'
  , byteString ", \"name\": "
    , char8 '"' <> byteString pcName <> char8 '"'
    , char8 '\n'
  , byteString ", \"portrait-id\": "
    , string8 $ show pcPortrait
    , char8 '\n'
  , byteString ", \"gender\": "
    , displayGender pcGender
    , char8 '\n'
  , byteString ", \"class\": "
    , string8 $ show pcClass
    , char8 '\n'
  , byteString ", \"race\": "
    , string8 $ show pcRace
    , char8 '\n'
  , byteString ", \"alignment\": "
    , displayAlignment pcAlign
    , char8 '\n'
  , byteString ", \"hp\": "
    , string8 $ show pcHp
    , char8 '\n'
  , byteString ", \"mob-info\":\n"
    , displayObjectInfo objInfo
    , char8 '\n'
  , byteString ", \"mob-id\": "
    , displayObjectId True objId
    , char8 '\n'
  , byteString ", \"mob-type\": \""
    , string8 $ typeName objType
    , byteString "\"\n, "
  , displayFields condNames fields
  , byteString "\n}\n"
  ]

displayGender :: Word32 -> Builder
displayGender = \case
  0 -> byteString "\"female\""
  1 -> byteString "\"male\""
  n -> string8 $ show n

displayAlignment :: Word32 -> Builder
displayAlignment = \case
  0 -> byteString "\"neutral\""
  1 -> byteString "\"lawful neutral\""
  2 -> byteString "\"chaotic neutral\""
  4 -> byteString "\"neutral good\""
  5 -> byteString "\"lawful good\""
  6 -> byteString "\"chaotic good\""
  8 -> byteString "\"neutral evil\""
  9 -> byteString "\"lawful evil\""
  10 -> byteString "\"chaotic evil\""
  n -> string8 $ show n

displayObjectInfo :: ObjectInfo -> Builder
displayObjectInfo (ObjInfo {..})
  = mconcat
  [ byteString "    { \"subtype\": "
  , string8 $ show subtype
  , byteString "\n    , \"protoId\": "
  , string8 $ show protoId
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

displayFields :: Map Word32 ByteString -> Map ObjectField Value -> Builder
displayFields condNames =
  intercalateMap "\n, " (uncurry $ displayField condNames) . Map.toList

displayField :: Map Word32 ByteString -> ObjectField -> Value -> Builder
displayField condNames fld val
  = mconcat
  [ byteString "\""
  , string8 $ fieldName fld
  , byteString "\": "
  , displayValue condNames val
  ]

displayValue :: Map Word32 ByteString -> Value -> Builder
displayValue condNames = \case
  Null -> byteString "null"
  W32 w -> string8 $ show w
  W64 w -> string8 $ show w
  I32 i -> string8 $ show i
  F32 f -> string8 $ show f
  B32 b -> displayBool b
  Obj u -> displayObjectId False u
  Loc l -> displayLoc l
  I32Arr is -> displayArray Nothing (string8 . show) is
  W32Arr ws -> displayArray Nothing (string8 . show) ws
  W64Arr ws -> displayArray Nothing (string8 . show) ws
  ObjArr us -> displayArray (Just 4) (displayObjectId False) us
  CondArr cs -> displayArray (Just 4) (displayCondition condNames) cs
  ScriptArr ss -> displayScriptArray ss
  StandptArr sps -> displayArray (Just 4) displayStandpoint sps
  SpellArr sps -> displayArray (Just 4) displaySpellData sps
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

displayCondition :: Map Word32 ByteString -> Word32 -> Builder
displayCondition condNames w
  | Just name <- Map.lookup w condNames =
    char8 '"' <> byteString name <> char8 '"'
  | otherwise = string8 $ show w

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

displaySpellData :: SpellData -> Builder
displaySpellData (Spell {..}) =
  mconcat
    [ byteString "{ \"spell-enum\": "
    , string8 $ show spellEnum
    , brk <> byteString ", \"spell-class\": "
    , string8 $ show spellClass
    , brk <> byteString ", \"spell-level\": "
    , string8 $ show spellLevel
    , brk <> byteString ", \"spell-type\": "
    , displaySpellType spellType
    , brk <> byteString ", \"spell-used\": "
    , displayBool spellUsed
    , brk <> byteString ", \"metamagic\": "
    , displayMetamagic spellMeta
    , brk <> byteString ", \"indicator1\": "
    , string8 $ show spellInd1
    , brk <> byteString ", \"indicator2\": "
    , string8 $ show spellInd2
    , brk <> byteString ", \"indicator3\": "
    , string8 $ show spellInd3
    , brk <> byteString "}"
    ]
  where
  brk = indent $ Just 6

displayMetamagic :: Metamagic -> Builder
displayMetamagic (Mm w) = string8 $ show w

displaySpellType :: SpellType -> Builder
displaySpellType = \case
  SpellNone -> byteString "\"none\""
  SpellKnown -> byteString "\"known\""
  SpellMemorized -> byteString "\"memorized\""
  SpellCast -> byteString "\"cast\""
  SpellAtWill -> byteString "\"at will\""

displayBool :: Bool -> Builder
displayBool = string8 . fmap toLower . show

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
 <> intercalate comma (f <$> Map.toList ss)
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

intercalateMap :: Monoid m => m -> (a -> m) -> [a] -> m
intercalateMap e f = mconcat . intersperse e . fmap f

indent :: Maybe Int -> Builder
indent = \case
  Nothing -> mempty
  Just ind -> mconcat $ char8 '\n' : replicate ind (char8 ' ')

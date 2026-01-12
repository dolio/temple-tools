
module Mob
  ( Array (..)
  , array
  , bitmap
  , Defaulted
  , finalize
  , Loc (..)
  , setLocX
  , setLocY
  , Offsets (..)
  , setOffX
  , setOffY
  , Mob (..)
  , setMobInfo
  , setMobId
  , setMobType
  , setMobField
  , MobDiff (..)
  , Player (..)
  , setPlayerFlags
  , setPlayerId
  , setPlayerName
  , setPlayerPortrait
  , setPlayerGender
  , setPlayerClass
  , setPlayerRace
  , setPlayerAlign
  , setPlayerHp
  , setPlayerData
  , ObjectId (..)
  , setObjectIdVariant
  , setObjectIdUUID
  , objectIdToFileName
  , ObjectInfo (..)
  , setOInfoSubtype
  , setOInfoProtoId
  , Standpoint (..)
  , setStdptMap
  , setStdptLoc
  , setStdptOff
  , setStdptJp
  , Script (..)
  , setScriptUnk
  , setScriptCounters
  , setScriptId
  , SpellType (..)
  , Metamagic (..)
  , SpellData (..)
  , setSpellEnum
  , setSpellClass
  , setSpellLevel
  , setSpellType
  , setSpellUsed
  , setSpellMeta
  , setSpellInd1
  , setSpellInd2
  , setSpellInd3
  , Value (..)
  , Waypoint (..)
  , setWayptFlags
  , setWayptLoc
  , setWayptOffs
  , setWayptRot
  , setWayptAnims
  , setWayptDelay
  , WaypointArr (..)
  , setWayptCount
  , setWayptExtra1
  , setWayptExtra2
  , setWayptExtra3
  , setWaypts
  , Format (..)
  ) where

import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Int
import Data.Map.Strict (Map, insert)
import Data.Monoid (Endo (..))
import Data.UUID
import Data.Word

import Bitmap

import Temple.Object.Field
import Temple.Object.Script
import Temple.Object.Skill
import Temple.Object.Type

class Defaulted t where defaultVal :: t

finalize :: Defaulted t => [Endo t] -> t
finalize = ($ defaultVal) . appEndo . mconcat

data Loc = L { locx, locy :: !Int32 } deriving (Eq, Ord, Show)
data Offsets = Off { offx, offy :: !Float } deriving (Eq, Ord, Show)

setLocX :: Int32 -> Endo Loc
setLocX x = Endo \l -> l { locx = x }

setLocY :: Int32 -> Endo Loc
setLocY y = Endo \l -> l { locy = y }

setOffX :: Float -> Endo Offsets
setOffX x = Endo \o -> o { offx = x }

setOffY :: Float -> Endo Offsets
setOffY y = Endo \o -> o { offy = y }

instance Defaulted Loc where defaultVal = L 0 0
instance Defaulted Offsets where defaultVal = Off 0 0

-- Standpoints are 32 bytes of actual data. The underlying array that stores
-- them is classified as a Word64 array. This might lead you to believe that
-- the size would be a multiple of 4, but actually it is a multiple of *10*
-- and each standpoint contains 6 words of padding that usually seems to be
-- zeroed. This isn't represented here because of the zeroing.
--
-- `jp` is some kind of "jump point" information. It and `mapInfo` might be
-- 4-byte values with 4 bytes of padding afterwards, but I'm uncertain.
data Standpoint
  = Stdpt
  { mapInfo :: !Word64
  , loc     :: !Loc
  , offsets :: !Offsets
  , jp      :: !Word64
  } deriving (Eq, Ord, Show)

setStdptMap :: Word64 -> Endo Standpoint
setStdptMap m = Endo \s -> s { mapInfo = m }

setStdptLoc :: Loc -> Endo Standpoint
setStdptLoc l = Endo \s -> s { loc = l }

setStdptOff :: Offsets -> Endo Standpoint
setStdptOff o = Endo \s -> s { offsets = o }

setStdptJp :: Word64 -> Endo Standpoint
setStdptJp j = Endo \s -> s { jp = j }

instance Defaulted Standpoint where
  defaultVal = Stdpt 0 defaultVal defaultVal 0

-- Waypoints are 64 byte chunks of information that are partitioned into 8
-- byte elements to encode as a Word64 array. These six fields seem to be the
-- actual data, and there are 7 4-byte words of padding at the end according
-- to Temple+. This padding seems to typically not be zeroed out, but it's
-- probably just garbage from uninitialized memory.
data Waypoint
  = Waypt
  { wayptFlags :: !Word32
  , wayptLoc   :: !Loc
  , wayptOffs  :: !Offsets
  , wayptRot   :: !Float
  , wayptAnims :: !Word64
  , wayptDelay :: !Word32
  } deriving (Eq, Ord, Show)

setWayptFlags :: Word32 -> Endo Waypoint
setWayptFlags f = Endo \w -> w { wayptFlags = f }

setWayptLoc :: Loc -> Endo Waypoint
setWayptLoc l = Endo \w -> w { wayptLoc = l }

setWayptOffs :: Offsets -> Endo Waypoint
setWayptOffs o = Endo \w -> w { wayptOffs = o }

setWayptRot :: Float -> Endo Waypoint
setWayptRot r = Endo \w -> w { wayptRot = r }

setWayptAnims :: Word64 -> Endo Waypoint
setWayptAnims a = Endo \w -> w { wayptAnims = a }

setWayptDelay :: Word32 -> Endo Waypoint
setWayptDelay d = Endo \w -> w { wayptDelay = d }

instance Defaulted Waypoint where
  defaultVal = Waypt 0 defaultVal defaultVal 0 0 0

-- Waypoint arrays are internally built on 8-byte word arrays, but the
-- information they represent has more structure. The basic array coding in
-- files just uses an element size of 8, however.
--
-- Each Waypoint (see above) requires 8 words to represent (64 bytes). So,
-- fully storing the waypoints requires a multiple of 8 entries. However,
-- the start of the array also contains two extra words worth of data, so the
-- total array size is 2 + 8*n where n is the number of waypoints.
--
-- Some information in World Build suggests that the first 4 bytes of this
-- extra stuff is a waypoint count. It does _not_ always match the actual
-- number of waypoints in the array, and any extra waypoints do seem to have
-- reasonable values. I'm not sure what the other 12 bytes are. It would be
-- unsurprising if it were just padding filled with garbage data.
data WaypointArr
  = Waypts
  { wayptCount  :: !Word32
  , wayptExtra1 :: !Word32
  , wayptExtra2 :: !Word32
  , wayptExtra3 :: !Word32
  , waypts      :: [Waypoint]
  } deriving (Eq, Show)

setWayptCount :: Word32 -> Endo WaypointArr
setWayptCount c = Endo \w -> w { wayptCount = c }

setWayptExtra1 :: Word32 -> Endo WaypointArr
setWayptExtra1 e = Endo \w -> w { wayptExtra1 = e }

setWayptExtra2 :: Word32 -> Endo WaypointArr
setWayptExtra2 e = Endo \w -> w { wayptExtra2 = e }

setWayptExtra3 :: Word32 -> Endo WaypointArr
setWayptExtra3 e = Endo \w -> w { wayptExtra3 = e }

setWaypts :: [Waypoint] -> Endo WaypointArr
setWaypts ws = Endo \w -> w { waypts = ws }

instance Defaulted WaypointArr where
  defaultVal = Waypts 0 0 0 0 []

-- Note on ToEE property arrays
-- ----------------------------
--
-- From what I can tell, every ToEE object property array is stored (in the
-- original game) as a blob of bytes in the following format:
--
--   |   4 bytes    |    4 bytes    |   4 bytes    |   variable  |
--   | Element Size | Element Count | Bitmap Index | Content ... |
--
-- Naturally, the variable part has length equal to the product of the frist
-- two parts. The bitmap id is an offset into a table of bitmaps, and the
-- bitmap tells you how the contiguous values in the blob are arranged into a
-- potentially sparse array.
--
-- For instance, the object script array would typically be sparse, because
-- most objects do not have scripts of every type installed. So, the script
-- array has a handful of script ids stored contiguously, plus a bitmap
-- specifying which actual scripts are specified. In the case of other arrays,
-- this will usually be a dense bitmap with a string of all 1s followed by a
-- string of all 0s padding it out.
--
-- The ToEE MOB format appears to just store these arrays by dumping the whole
-- blob, followed by the bitmap. This means that the index into the bitmap
-- side table is dumped with it. But this is transient information. It is the
-- position in the table that the bitmap was stored when the file was written.
-- But when the file is loaded again, it appears that a new index is allocated
-- for the bitmap, and the old index is just overwritten. This makes sense,
-- because otherwise it would rquire assigning unique positions in the table
-- for every possible object in the game, which sounds like a nightmare.
--
-- Point being, this field in the MOB format is garbage. I suspect it exists
-- only because it was easier for them to dump the whole chunk of memory
-- directly to a file. It should not matter if something different gets
-- written there when modifying a file.
data Array e
  = Dense { content :: [e] }
  | Sparse
  { content :: [e]
  , _bitmap :: Bitmap
  } deriving (Eq, Show)

-- Smart constructor that detects array density based on a bitmap. The
-- underlying size of the array is provided since the element list may have
-- been decoded from the underlying representation and contain fewer values.
array :: Int -> [e] -> Bitmap -> Array e
array sz els bm
  | isDense sz bm = Dense els
  | otherwise = Sparse els bm

-- Gets a bitmap appropriate for an array. For a sparse array, this is just
-- stored. For a dense array, it can be reconstructed from the content.
bitmap :: Array e -> Bitmap
bitmap (Sparse {..}) = _bitmap
bitmap (Dense {..}) = denseBitmap (length content)

data Script
  = Script
  { scrUnknown  :: !Word32
  , scrCounters :: !Word32
  , scrId       :: !Word32
  } deriving (Eq, Ord, Show)

setScriptUnk :: Word32 -> Endo Script
setScriptUnk u = Endo \s -> s { scrUnknown = u }

setScriptCounters :: Word32 -> Endo Script
setScriptCounters c = Endo \s -> s { scrCounters = c }

setScriptId :: Word32 -> Endo Script
setScriptId i = Endo \s -> s { scrId = i }


instance Defaulted Script where
  defaultVal = Script 0 0 0

data SpellType
  = SpellNone
  | SpellKnown
  | SpellMemorized
  | SpellCast
  | SpellAtWill
  deriving (Bounded, Enum, Eq, Ord, Show)

newtype Metamagic = Mm Word32
  deriving (Eq, Ord, Show)

data SpellData
  = Spell
  { spellEnum  :: Word32
  , spellClass :: Word32
  , spellLevel :: Word32
  , spellType  :: SpellType
  , spellUsed  :: Bool
  , spellMeta  :: Metamagic
  -- something to do with metamagic indicators
  , spellInd1  :: Word32
  , spellInd2  :: Word32
  , spellInd3  :: Word32
  } deriving (Eq, Show)

setSpellEnum :: Word32 -> Endo SpellData
setSpellEnum w = Endo \s -> s { spellEnum = w }

setSpellClass :: Word32 -> Endo SpellData
setSpellClass w = Endo \s -> s { spellClass = w }

setSpellLevel :: Word32 -> Endo SpellData
setSpellLevel w = Endo \s -> s { spellLevel = w }

setSpellType :: SpellType -> Endo SpellData
setSpellType t = Endo \s -> s { spellType = t }

setSpellUsed :: Bool -> Endo SpellData
setSpellUsed b = Endo \s -> s { spellUsed = b }

setSpellMeta :: Metamagic -> Endo SpellData
setSpellMeta m = Endo \s -> s { spellMeta = m }

setSpellInd1 :: Word32 -> Endo SpellData
setSpellInd1 w = Endo \s -> s { spellInd1 = w }

setSpellInd2 :: Word32 -> Endo SpellData
setSpellInd2 w = Endo \s -> s { spellInd2 = w }

setSpellInd3 :: Word32 -> Endo SpellData
setSpellInd3 w = Endo \s -> s { spellInd3 = w }

instance Defaulted SpellData where
  defaultVal = Spell 0 0 0 SpellNone False (Mm 0) 0 0 0

data Value
  = W32 !Word32
  | Loc !Loc
  | W64 !Word64
  | I32 !Int32
  | F32 !Float
  | B32 !Bool
  | Obj !ObjectId
  | I32Arr (Array Int32)
  | W32Arr (Array Word32)
  | W64Arr (Array Word64)
  | CondArr (Array Word32)
  | ObjArr (Array ObjectId)
  | ScriptArr (Map ObjectScript Script)
  | StandptArr (Array Standpoint)
  | SkillArr (Map Skill Word32)
  | WayptArr WaypointArr
  | String !ByteString
  | SpellArr (Array SpellData)
  | Null
  deriving (Eq, Show)

-- This is an identifier for a ToEE object. The actual identifier is the UUID,
-- which always seems to be in MS GUID format, which is variant 2. There is
-- also 8 bits of 'variant' information or something. For most mobs this is
-- just 2, and the lowest 2 bytes always seem to be 2 in practice, but
-- sometimes the higher 6 bytes are filled with 0xcd for unknown reasons.
--
-- I think it's quite likely that only the first 2 bytes are meant to specify
-- the variant, and the 6 after are padding, to match up with ObjectInfo
-- below. That would mean that the 0xcd stuff is just junk of some sort
-- (although suspiciously regular junk).
data ObjectId
  = ObjId
  { variant :: !Word64
  , uuid    :: !UUID
  } deriving (Eq, Ord, Show)

setObjectIdVariant :: Word64 -> Endo ObjectId
setObjectIdVariant v = Endo \i -> i { variant = v }

setObjectIdUUID :: UUID -> Endo ObjectId
setObjectIdUUID u = Endo \i -> i { uuid = u }

objectIdToFileName :: ObjectId -> String
objectIdToFileName (ObjId {..}) = "G_" ++ tweak (toString uuid) where
  tweak [] = []
  tweak ('-':cs) = '_' : tweak cs
  tweak (c:cs) = toUpper c : tweak cs

instance Defaulted ObjectId where
  defaultVal = ObjId 0 nil

-- This stores object information about a mob. The known fields are assembled
-- from a combination of testing and reading information from the DLL, Temple+
-- and world builder.
--
-- The first field stores the 'subtype' of the object in 16 bits. This is some
-- indication of how the object is created, but in all MOB files I've seen,
-- it's 1, which means 'prototype.' The next 6 bytes are unknown, but I think
-- it's quite likely it's padding.
--
-- Next is 32 bits for the prototype id, followed by 12 bits that is likely
-- padding as well. These and the above padding bits often are not 0, but this
-- is likely just junk from uninitialized memory.
--
-- The reason behind this is that it appears to be that these `ObjectInfo`
-- structures were wanted to be the same size as `ObjectId` structures, which
-- are 24 bytes. The prototype id overlaps with the GUID part, and the subtype
-- overlaps with the variant part. This leaves a lot of padding for this
-- structure, but much less for ObjectId.
--
-- WorldBuilder stores compatibility information in the padding after subtype,
-- but I think the game probably doesn't care about that part.
data ObjectInfo
  = ObjInfo
  { subtype    :: !Word16
  , protoId    :: !Word32
  } deriving (Eq, Ord, Show)

setOInfoSubtype :: Word16 -> Endo ObjectInfo
setOInfoSubtype s = Endo \o -> o { subtype = s }

setOInfoProtoId :: Word32 -> Endo ObjectInfo
setOInfoProtoId p = Endo \o -> o { protoId = p }

instance Defaulted ObjectInfo where
  defaultVal = ObjInfo 0 0

data Mob
  = Mob
  { objInfo :: !ObjectInfo
  , objId   :: !ObjectId
  , objType :: !ObjectType
  , fields  :: Map ObjectField Value
  } deriving (Eq, Show)

-- A mob diff stores differences from an original mob file. It doesn't appear
-- that they're able to be interpreted stand-alone. The original object
-- (identified by UUID) needs to be accessed to determine the object type, and
-- thus the number of bitmap fields.
data MobDiff = MobDiff { diffFields :: Map ObjectField Value }
  deriving (Eq, Show)

setMobInfo :: ObjectInfo -> Endo Mob
setMobInfo o = Endo \m -> m { objInfo = o }

setMobId :: ObjectId -> Endo Mob
setMobId i = Endo \m -> m { objId = i }

setMobType :: ObjectType -> Endo Mob
setMobType t = Endo \m -> m { objType = t }

setMobField :: ObjectField -> Value -> Endo Mob
setMobField f v = Endo \m -> m { fields = insert f v $ fields m }

instance Defaulted Mob where
  defaultVal = Mob  defaultVal defaultVal Portal mempty

-- A created character is some extra info wrapped around a Mob containing the
-- actual character data.
data Player
  = Player
  { pcFlags    :: Word32
  , pcId       :: ObjectId
  , pcName     :: ByteString
  , pcPortrait :: Word32
  , pcGender   :: Word32
  , pcClass    :: Word32
  , pcRace     :: Word32
  , pcAlign    :: Word32
  , pcHp       :: Word32
  , pcData     :: Mob
  }

setPlayerFlags :: Word32 -> Endo Player
setPlayerFlags w = Endo \p -> p { pcFlags = w }

setPlayerId :: ObjectId -> Endo Player
setPlayerId i = Endo \p -> p { pcId = i }

setPlayerName :: ByteString -> Endo Player
setPlayerName nm = Endo \p -> p { pcName = nm }

setPlayerPortrait :: Word32 -> Endo Player
setPlayerPortrait w = Endo \p -> p { pcPortrait = w }

setPlayerGender :: Word32 -> Endo Player
setPlayerGender g = Endo \p -> p { pcGender = g }

setPlayerClass :: Word32 -> Endo Player
setPlayerClass c = Endo \p -> p { pcClass = c }

setPlayerRace :: Word32 -> Endo Player
setPlayerRace r = Endo \p -> p { pcRace = r }

setPlayerAlign :: Word32 -> Endo Player
setPlayerAlign a = Endo \p -> p { pcAlign = a }

setPlayerHp :: Word32 -> Endo Player
setPlayerHp h = Endo \p -> p { pcHp = h }

setPlayerData :: Endo Mob -> Endo Player
setPlayerData em = Endo \p -> p { pcData = appEndo em (pcData p) }

instance Defaulted Player where
  defaultVal = Player 0 defaultVal "" 0 0 0 0 0 0 defaultVal

-- Object data is saved in slightly different ways in different files. This
-- type represents those variations in the format.
data Format = MobFile | DiffFile deriving (Eq, Ord, Show)

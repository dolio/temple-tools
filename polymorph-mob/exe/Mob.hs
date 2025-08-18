
module Mob
  ( Array (..)
  , array
  , bitmap
  , ArrayPostamble (..)
  , Loc (..)
  , Mob (..)
  , ObjectId (..)
  , ObjectInfo (..)
  , Offsets (..)
  , Standpoint (..)
  , Script (..)
  , Value (..)
  , Waypoint (..)
  , WaypointArr (..)
  ) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Map.Strict (Map)
import Data.UUID
import Data.Word

import Bitmap

import Temple.Objects.Spec

data Loc = L { x, y :: !Int32 } deriving (Eq, Ord, Show)
data Offsets = Off { offx, offy :: !Float } deriving (Eq, Ord, Show)

data Standpoint
  = Stdpt
  { mapInfo :: !Word64
  , loc     :: !Loc
  , offsets :: !Offsets
  , jp      :: !Word64
  } deriving (Eq, Ord, Show)

data Waypoint
  = Waypt
  { wayptFlags :: !Word32
  , wayptLoc   :: !Loc
  , wayptOffs  :: !Offsets
  , wayptRot   :: !Float
  , wayptAnims :: !Word64
  , wayptDelay :: !Word32
  , wayptExtra :: [Word32]
  } deriving (Eq, Ord, Show)

data ArrayPostamble = Post [Word32]
  deriving (Eq, Ord, Show)

data WaypointArr
  = Waypts
  { wayptCount  :: !Word32
  , wayptExtra1 :: !Word32
  , wayptExtra2 :: !Word32
  , wayptExtra3 :: !Word32
  , waypts      :: [Waypoint]
  } deriving (Eq, Show)

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
  | ObjArr (Array ObjectId)
  | ScriptArr (Map ObjectScript Script)
  | StandptArr (Array Standpoint)
  | WayptArr WaypointArr
  | String !ByteString
  | Null
  deriving (Eq, Show)

-- This is an identifier for a ToEE object. The actual identifier is the UUID,
-- which always seems to be in MS GUID format, which is variant 2. There is
-- also 8 bits of 'variant' information or something. For most mobs this is
-- just 2, and the lowest 2 bytes always seem to be 2 in practice, but
-- sometimes the higher 6 bytes are filled with 0xcd for unknown reasons.
data ObjectId
  = ObjId
  { variant :: !Word64
  , uuid    :: !UUID
  } deriving (Eq, Ord, Show)

-- This stores object information about a mob. The one field known for sure is
-- `protoId`.
--
-- I have also guessed where the 'subtype' information is stored, but I'm not
-- 100% sure it's the correct position. Unfortunately I don't have access to
-- the corresopnding ToEE struct. The guess is based on WorldBuilder always
-- writing a 1 here, and all .mob files also having a 1, which is a
-- 'prototype' object.
--
-- The 'compat' was used for that by world builder, but I'm not sure what else
-- might be there. Some of these fields might even be pointer addresses or the
-- like.
data ObjectInfo
  = ObjInfo
  { subtype    :: !Word16
  , compat     :: !Word32
  , oiUnknown2 :: !Word16
  , protoId    :: !Word32
  , oiUnknown3 :: !Word32
  , oiUnknown4 :: !Word32
  , oiUnknown5 :: !Word32
  } deriving (Eq, Ord, Show)

data Mob
  = Mob
  { objInfo :: !ObjectInfo
  , objId   :: !ObjectId
  , objType :: !ObjectType
  , fields  :: Map ObjectField Value
  } deriving (Eq, Show)

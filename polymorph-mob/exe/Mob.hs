
module Mob
  ( Loc (..)
  , Mob (..)
  , Offsets (..)
  , Standpoint (..)
  , Value (..)
  , VUUID (..)
  , Waypoint (..)
  , WaypointArr (..)
  ) where

import Data.Int
import Data.Map.Strict (Map)
import Data.UUID
import Data.Word

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

data WaypointArr
  = Waypts
  { wayptCount  :: !Word32
  , wayptExtra1 :: !Word32
  , wayptExtra2 :: !Word32
  , wayptExtra3 :: !Word32
  , waypts      :: [Waypoint]
  }

data Value
  = W32 !Word32
  | Loc !Loc
  | W64 !Word64
  | I32 !Int32
  | F32 !Float
  | B32 !Bool
  | UID !VUUID
  | I32Arr [Int32]
  | W32Arr [Word32]
  | W64Arr [Word64]
  | ObjArr [VUUID]
  | ScriptArr [(Word32, Word32, Word32)]
  | StandptArr [Standpoint]
  | WayptArr WaypointArr

data VUUID
  = VUUID
  { variant :: !Word64
  , uuid    :: !UUID
  } deriving (Eq, Ord, Show)

data Mob
  = Mob
  { pad0 :: !Word16
  , compat :: !Word32
  , pad1 :: !Word16
  , protoId :: !Word32
  , vuuid :: !VUUID
  , objType :: !ObjectType
  , fields :: Map ObjectField Value
  }

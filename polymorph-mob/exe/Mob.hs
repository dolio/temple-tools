
module Mob
  ( Loc (..)
  , Mob (..)
  , Offsets (..)
  , StandPoint (..)
  , Value (..)
  , VUUID (..)
  ) where

import Data.Int
import Data.Map.Strict (Map)
import Data.UUID
import Data.Word

import Temple.Objects.Spec

data Loc = L { x, y :: !Int32 } deriving (Eq, Ord, Show)
data Offsets = Off { offx, offy :: !Float } deriving (Eq, Ord, Show)

data StandPoint
  = StdPt
  { mapInfo :: !Word64
  , loc     :: !Loc
  , offsets :: !Offsets
  , jp      :: !Word64
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
  | StandPtArr [StandPoint]

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

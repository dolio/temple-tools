
module Mob
  ( Value(..)
  , Mob (..)
  ) where

import Data.Int
import Data.Map.Strict (Map)
import Data.UUID
import Data.Word

import Temple.Objects.Spec

data Value
  = W32 !Word32
  | W32x2 !Word32 !Word32
  | W64 !Word64
  | I32 !Int32
  | F32 !Float
  | B32 !Bool
  | UID !UUID

data Mob
  = Mob
  { pad0 :: !Word16
  , compat :: !Word32
  , pad1 :: !Word16
  , protoId :: !Word32
  , uuid :: !UUID
  , objType :: !ObjectType
  , fields :: Map ObjectField Value
  }


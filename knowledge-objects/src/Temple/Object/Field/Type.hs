
module Temple.Object.Field.Type where

-- The type of data that occurs in a field
--
-- Note: Temple+ says location fields are Int64, because the location is
-- normally packed. But it seems to be stored differently in e.g. MOB files,
-- so it's worth distinguishing.
data FieldType
  = BeginF      -- beginning of section
  | EndF        -- ending of section
  | NoneF       -- probably shouldn't be used for anything, like the above
  | W32F        -- unsigned 32-bit integer
  | W64F        -- unsigned 64-bit integer
  | I32F        -- signed 32-bit integer
  | F32F        -- 32-bit float
  | B32F        -- 32-bit boolean representation
  | LocF        -- location, 2 32-bit integers
  | ObjF        -- object field (probably UUID in serialized cases)
  | StringF     -- string
  | W32ArrF     -- array of unsigned 32-bit integers
  | W64ArrF     -- array of unsigned 64-bit integers
  | CondArrF    -- array of conditions
  | ObjArrF     -- array of objects
  | StandptArrF -- array of standpoints
  | WayptArrF   -- array of waypoints
  | AbilityArrF -- array of ability scores
  | ScriptArrF  -- array of scripts (numbers, probably)
  | SpellArrF   -- array of spell entries
  | SkillArrF   -- array of skills
  deriving (Eq, Ord, Show)

-- Classifies field types that hold arrays
isArray :: FieldType -> Bool
isArray = \case
  W32ArrF -> True
  W64ArrF -> True
  CondArrF -> True
  ObjArrF -> True
  StandptArrF -> True
  WayptArrF -> True
  AbilityArrF -> True
  ScriptArrF -> True
  SpellArrF -> True
  SkillArrF -> True
  _ -> False

-- Classifies field types that are just markers, with no associated data.
isMarker :: FieldType -> Bool
isMarker = \case
  BeginF -> True
  EndF -> True
  NoneF -> True
  _ -> False

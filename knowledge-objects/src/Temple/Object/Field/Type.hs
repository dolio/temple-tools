
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
  deriving (Eq, Ord, Show)


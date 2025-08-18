
module Temple.Object.Field.Generic where

import Temple.Object.Field.Type

-- 'Generic' is (I think) a category for items that don't fit into one of
-- the above categories. A common use is to spawn one as the anchor for an
-- AoE spell.
data GenericField
  = GenericBegin
  | GenericFlags
  | GenericUsageBonus
  | GenericUsageCountRemaining
  | GenericPadIntArr1
  | GenericPadInt64Arr1
  | GenericEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum GenericField where
  fromEnum = \case
    GenericBegin               -> 276
    GenericFlags               -> 277
    GenericUsageBonus          -> 278
    GenericUsageCountRemaining -> 279
    GenericPadIntArr1          -> 280
    GenericPadInt64Arr1        -> 281
    GenericEnd                 -> 282

  toEnum = \case
    276 -> GenericBegin
    277 -> GenericFlags
    278 -> GenericUsageBonus
    279 -> GenericUsageCountRemaining
    280 -> GenericPadIntArr1
    281 -> GenericPadInt64Arr1
    282 -> GenericEnd
    n -> error $ "toEnum @GenericField: bad value: " ++ show n

genericFieldName :: GenericField -> String
genericFieldName = \case
  GenericBegin               -> "obj_f_generic_begin"
  GenericFlags               -> "obj_f_generic_flags"
  GenericUsageBonus          -> "obj_f_generic_usage_bonus"
  GenericUsageCountRemaining -> "obj_f_generic_usage_count_remaining"
  GenericPadIntArr1          -> "obj_f_generic_pad_ias_1"
  GenericPadInt64Arr1        -> "obj_f_generic_pad_i64as_1"
  GenericEnd                 -> "obj_f_generic_end"

genericFieldType :: GenericField -> FieldType
genericFieldType = \case
  GenericBegin -> BeginF
  GenericFlags -> W32F
  GenericUsageBonus -> W32F
  GenericUsageCountRemaining -> W32F
  GenericPadIntArr1 -> W32ArrF
  GenericPadInt64Arr1 -> W64ArrF
  GenericEnd -> EndF


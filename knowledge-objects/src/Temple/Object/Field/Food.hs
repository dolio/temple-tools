
module Temple.Object.Field.Food where

import Temple.Object.Field.Type

data FoodField
  = FoodBegin
  | FoodFlags
  | FoodPadInt1
  | FoodPadInt2
  | FoodPadIntArr1
  | FoodPadInt64Arr1
  | FoodEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum FoodField where
  fromEnum = \case
    FoodBegin        -> 241
    FoodFlags        -> 242
    FoodPadInt1      -> 243
    FoodPadInt2      -> 244
    FoodPadIntArr1   -> 245
    FoodPadInt64Arr1 -> 246
    FoodEnd          -> 247

  toEnum = \case
    241 -> FoodBegin
    242 -> FoodFlags
    243 -> FoodPadInt1
    244 -> FoodPadInt2
    245 -> FoodPadIntArr1
    246 -> FoodPadInt64Arr1
    247 -> FoodEnd
    n -> error $ "toEnum @FoodField: bad value: " ++ show n

foodFieldName :: FoodField -> String
foodFieldName = \case
  FoodBegin        -> "obj_f_food_begin"
  FoodFlags        -> "obj_f_food_flags"
  FoodPadInt1      -> "obj_f_food_pad_i_1"
  FoodPadInt2      -> "obj_f_food_pad_i_2"
  FoodPadIntArr1   -> "obj_f_food_pad_ias_1"
  FoodPadInt64Arr1 -> "obj_f_food_pad_i64as_1"
  FoodEnd          -> "obj_f_food_end"

foodFieldType :: FoodField -> FieldType
foodFieldType = \case
  FoodBegin -> BeginF
  FoodFlags -> W32F
  FoodPadInt1 -> W32F
  FoodPadInt2 -> W32F
  FoodPadIntArr1 -> W32ArrF
  FoodPadInt64Arr1 -> W64ArrF
  FoodEnd -> EndF


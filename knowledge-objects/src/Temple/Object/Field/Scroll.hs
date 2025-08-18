
module Temple.Object.Field.Scroll where

import Temple.Object.Field.Type

data ScrollField
  = ScrollBegin
  | ScrollFlags
  | ScrollPadInt1
  | ScrollPadInt2
  | ScrollPadIntArr1
  | ScrollPadInt64Arr1
  | ScrollEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum ScrollField where
  fromEnum = \case
    ScrollBegin        -> 248
    ScrollFlags        -> 249
    ScrollPadInt1      -> 250
    ScrollPadInt2      -> 251
    ScrollPadIntArr1   -> 252
    ScrollPadInt64Arr1 -> 253
    ScrollEnd          -> 254
  toEnum = \case
    248 -> ScrollBegin
    249 -> ScrollFlags
    250 -> ScrollPadInt1
    251 -> ScrollPadInt2
    252 -> ScrollPadIntArr1
    253 -> ScrollPadInt64Arr1
    254 -> ScrollEnd
    n -> error $ "toEnum @ScrollField: bad value: " ++ show n

scrollFieldName :: ScrollField -> String
scrollFieldName = \case
  ScrollBegin        -> "obj_f_scroll_begin"
  ScrollFlags        -> "obj_f_scroll_flags"
  ScrollPadInt1      -> "obj_f_scroll_pad_i_1"
  ScrollPadInt2      -> "obj_f_scroll_pad_i_2"
  ScrollPadIntArr1   -> "obj_f_scroll_pad_ias_1"
  ScrollPadInt64Arr1 -> "obj_f_scroll_pad_i64as_1"
  ScrollEnd          -> "obj_f_scroll_end"

scrollFieldType :: ScrollField -> FieldType
scrollFieldType = \case
  ScrollBegin -> BeginF
  ScrollFlags -> W32F
  ScrollPadInt1 -> W32F
  ScrollPadInt2 -> W32F
  ScrollPadIntArr1 -> W32ArrF
  ScrollPadInt64Arr1 -> W64ArrF
  ScrollEnd -> EndF


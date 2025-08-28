
module Temple.Object.Field.Written where

import Temple.Object.Field.Class
import Temple.Object.Field.Type

data WrittenField
  = WrittenBegin
  | WrittenFlags
  | WrittenSubtype
  | WrittenTextStartLine
  | WrittenTextEndLine
  | WrittenPadInt1
  | WrittenPadInt2
  | WrittenPadIntArr1
  | WrittenPadInt64Arr1
  | WrittenEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum WrittenField where
  fromEnum = \case
    WrittenBegin         -> 262
    WrittenFlags         -> 263
    WrittenSubtype       -> 264
    WrittenTextStartLine -> 265
    WrittenTextEndLine   -> 266
    WrittenPadInt1       -> 267
    WrittenPadInt2       -> 268
    WrittenPadIntArr1    -> 269
    WrittenPadInt64Arr1  -> 270
    WrittenEnd           -> 271

  toEnum = \case
    262 -> WrittenBegin
    263 -> WrittenFlags
    264 -> WrittenSubtype
    265 -> WrittenTextStartLine
    266 -> WrittenTextEndLine
    267 -> WrittenPadInt1
    268 -> WrittenPadInt2
    269 -> WrittenPadIntArr1
    270 -> WrittenPadInt64Arr1
    271 -> WrittenEnd
    n -> error $ "toEnum @WrittenField: bad value: " ++ show n

  enumFrom n = enumFromTo n maxBound
  enumFromThen m n = enumFromThenTo m n maxBound

instance Field WrittenField where
  fieldName = \case
    WrittenBegin         -> "obj_f_written_begin"
    WrittenFlags         -> "obj_f_written_flags"
    WrittenSubtype       -> "obj_f_written_subtype"
    WrittenTextStartLine -> "obj_f_written_text_start_line"
    WrittenTextEndLine   -> "obj_f_written_text_end_line"
    WrittenPadInt1       -> "obj_f_written_pad_i_1"
    WrittenPadInt2       -> "obj_f_written_pad_i_2"
    WrittenPadIntArr1    -> "obj_f_written_pad_ias_1"
    WrittenPadInt64Arr1  -> "obj_f_written_pad_i64as_1"
    WrittenEnd           -> "obj_f_written_end"

  fieldType = \case
    WrittenBegin -> BeginF
    WrittenFlags -> W32F
    WrittenSubtype -> W32F
    WrittenTextStartLine -> W32F
    WrittenTextEndLine -> W32F
    WrittenPadInt1 -> W32F
    WrittenPadInt2 -> W32F
    WrittenPadIntArr1 -> W32ArrF
    WrittenPadInt64Arr1 -> W64ArrF
    WrittenEnd -> EndF

  isPadding = \case
    WrittenPadInt1 -> True
    WrittenPadInt2 -> True
    WrittenPadIntArr1 -> True
    WrittenPadInt64Arr1 -> True
    _ -> False

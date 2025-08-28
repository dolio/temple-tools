
module Temple.Object.Field.Trap where

import Temple.Object.Field.Class
import Temple.Object.Field.Type

data TrapField
  = TrapBegin
  | TrapFlags
  | TrapDifficulty
  | TrapPadInt2
  | TrapPadIntArr1
  | TrapPadInt64Arr1
  | TrapEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum TrapField where
  fromEnum = \case
    TrapBegin        -> 398
    TrapFlags        -> 399
    TrapDifficulty   -> 400
    TrapPadInt2      -> 401
    TrapPadIntArr1   -> 402
    TrapPadInt64Arr1 -> 403
    TrapEnd          -> 404

  toEnum = \case
    398 -> TrapBegin
    399 -> TrapFlags
    400 -> TrapDifficulty
    401 -> TrapPadInt2
    402 -> TrapPadIntArr1
    403 -> TrapPadInt64Arr1
    404 -> TrapEnd
    n -> error $ "toEnum @TrapField: bad value: " ++ show n

  enumFrom n = enumFromTo n maxBound
  enumFromThen m n = enumFromThenTo m n maxBound

instance Field TrapField where
  fieldName = \case
    TrapBegin                -> "obj_f_trap_begin"
    TrapFlags                -> "obj_f_trap_flags"
    TrapDifficulty           -> "obj_f_trap_difficulty"
    TrapPadInt2              -> "obj_f_trap_pad_i_2"
    TrapPadIntArr1           -> "obj_f_trap_pad_ias_1"
    TrapPadInt64Arr1         -> "obj_f_trap_pad_i64as_1"
    TrapEnd                  -> "obj_f_trap_end"

  fieldType = \case
    TrapBegin -> BeginF
    TrapFlags -> W32F
    TrapDifficulty -> W32F
    TrapPadInt2 -> W32F
    TrapPadIntArr1 -> W32ArrF
    TrapPadInt64Arr1 -> W64ArrF
    TrapEnd -> EndF

  isPadding = \case
    TrapPadInt2 -> True
    TrapPadIntArr1 -> True
    TrapPadInt64Arr1 -> True
    _ -> False

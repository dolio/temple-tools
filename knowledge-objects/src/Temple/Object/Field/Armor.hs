
module Temple.Object.Field.Armor where

import Temple.Object.Field.Class
import Temple.Object.Field.Type

data ArmorField
  = ArmorBegin
  | ArmorFlags
  | ArmorAcAdj
  | ArmorMaxDexBonus
  | ArmorArcaneSpellFailure
  | ArmorArmorCheckPenalty
  | ArmorPadInt1
  | ArmorPadIntArr1
  | ArmorPadInt64Arr1
  | ArmorEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum ArmorField where
  fromEnum = \case
    ArmorBegin              -> 219
    ArmorFlags              -> 220
    ArmorAcAdj              -> 221
    ArmorMaxDexBonus        -> 222
    ArmorArcaneSpellFailure -> 223
    ArmorArmorCheckPenalty  -> 224
    ArmorPadInt1            -> 225
    ArmorPadIntArr1         -> 226
    ArmorPadInt64Arr1       -> 227
    ArmorEnd                -> 228

  toEnum = \case
    219 -> ArmorBegin
    220 -> ArmorFlags
    221 -> ArmorAcAdj
    222 -> ArmorMaxDexBonus
    223 -> ArmorArcaneSpellFailure
    224 -> ArmorArmorCheckPenalty
    225 -> ArmorPadInt1
    226 -> ArmorPadIntArr1
    227 -> ArmorPadInt64Arr1
    228 -> ArmorEnd
    n -> error $ "toEnum @ArmorField: bad value: " ++ show n

  enumFrom n = enumFromTo n maxBound
  enumFromThen m n = enumFromThenTo m n maxBound

instance Field ArmorField where
  fieldName = \case
    ArmorBegin              -> "obj_f_armor_begin"
    ArmorFlags              -> "obj_f_armor_flags"
    ArmorAcAdj              -> "obj_f_armor_ac_adj"
    ArmorMaxDexBonus        -> "obj_f_armor_max_dex_bonus"
    ArmorArcaneSpellFailure -> "obj_f_armor_arcane_spell_failure"
    ArmorArmorCheckPenalty  -> "obj_f_armor_armor_check_penalty"
    ArmorPadInt1            -> "obj_f_armor_pad_i_1"
    ArmorPadIntArr1         -> "obj_f_armor_pad_ias_1"
    ArmorPadInt64Arr1       -> "obj_f_armor_pad_i64as_1"
    ArmorEnd                -> "obj_f_armor_end"

  fieldType = \case
    ArmorBegin -> BeginF
    ArmorFlags -> W32F
    ArmorAcAdj -> W32F
    ArmorMaxDexBonus -> W32F
    ArmorArcaneSpellFailure -> W32F
    ArmorArmorCheckPenalty -> W32F
    ArmorPadInt1 -> W32F
    ArmorPadIntArr1 -> W32ArrF
    ArmorPadInt64Arr1 -> W64ArrF
    ArmorEnd -> EndF

  isPadding = \case
    ArmorPadInt1 -> True
    ArmorPadIntArr1 -> True
    ArmorPadInt64Arr1 -> True
    _ -> False

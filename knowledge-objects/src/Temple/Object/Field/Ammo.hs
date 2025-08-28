
module Temple.Object.Field.Ammo where

import Temple.Object.Field.Class
import Temple.Object.Field.Type

data AmmoField
  = AmmoBegin
  | AmmoFlags
  | AmmoQuantity
  | AmmoType
  | AmmoPadInt1
  | AmmoPadInt2
  | AmmoPadObj1
  | AmmoPadIntArr1
  | AmmoPadInt64Arr1
  | AmmoEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum AmmoField where
  fromEnum = \case
    AmmoBegin        -> 209
    AmmoFlags        -> 210
    AmmoQuantity     -> 211
    AmmoType         -> 212
    AmmoPadInt1      -> 213
    AmmoPadInt2      -> 214
    AmmoPadObj1      -> 215
    AmmoPadIntArr1   -> 216
    AmmoPadInt64Arr1 -> 217
    AmmoEnd          -> 218

  toEnum = \case
    209 -> AmmoBegin
    210 -> AmmoFlags
    211 -> AmmoQuantity
    212 -> AmmoType
    213 -> AmmoPadInt1
    214 -> AmmoPadInt2
    215 -> AmmoPadObj1
    216 -> AmmoPadIntArr1
    217 -> AmmoPadInt64Arr1
    218 -> AmmoEnd
    n -> error $ "toEnum @AmmoField: bad value: " ++ show n

  enumFrom n = enumFromTo n maxBound
  enumFromThen m n = enumFromThenTo m n maxBound

instance Field AmmoField where
  fieldName = \case
    AmmoBegin        -> "obj_f_ammo_begin"
    AmmoFlags        -> "obj_f_ammo_flags"
    AmmoQuantity     -> "obj_f_ammo_quantity"
    AmmoType         -> "obj_f_ammo_type"
    AmmoPadInt1      -> "obj_f_ammo_pad_i_1"
    AmmoPadInt2      -> "obj_f_ammo_pad_i_2"
    AmmoPadObj1      -> "obj_f_ammo_pad_obj_1"
    AmmoPadIntArr1   -> "obj_f_ammo_pad_ias_1"
    AmmoPadInt64Arr1 -> "obj_f_ammo_pad_i64as_1"
    AmmoEnd          -> "obj_f_ammo_end"

  fieldType = \case
    AmmoBegin -> BeginF
    AmmoFlags -> W32F
    AmmoQuantity -> W32F
    AmmoType -> W32F
    AmmoPadInt1 -> W32F
    AmmoPadInt2 -> W32F
    AmmoPadObj1 -> ObjF
    AmmoPadIntArr1 -> W32ArrF
    AmmoPadInt64Arr1 -> W64ArrF
    AmmoEnd -> EndF

  isPadding = \case
    AmmoPadInt1 -> True
    AmmoPadInt2 -> True
    AmmoPadObj1 -> True
    AmmoPadIntArr1 -> True
    AmmoPadInt64Arr1 -> True
    _ -> False

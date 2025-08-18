
module Temple.Object.Field.Weapon where

import Temple.Object.Field.Type

data WeaponField
  = WeaponBegin
  | WeaponFlags
  | WeaponRange
  | WeaponAmmoType
  | WeaponAmmoConsumption
  | WeaponMissileAid
  | WeaponCritHitChart
  | WeaponAttacktype
  | WeaponDamageDice
  | WeaponAnimtype
  | WeaponType
  | WeaponCritRange
  | WeaponPadInt1
  | WeaponPadInt2
  | WeaponPadObj1
  | WeaponPadObj2
  | WeaponPadObj3
  | WeaponPadObj4
  | WeaponPadObj5
  | WeaponPadIntArr1
  | WeaponPadInt64Arr1
  | WeaponEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum WeaponField where
  fromEnum = \case
    WeaponBegin           -> 187
    WeaponFlags           -> 188
    WeaponRange           -> 189
    WeaponAmmoType        -> 190
    WeaponAmmoConsumption -> 191
    WeaponMissileAid      -> 192
    WeaponCritHitChart    -> 193
    WeaponAttacktype      -> 194
    WeaponDamageDice      -> 195
    WeaponAnimtype        -> 196
    WeaponType            -> 197
    WeaponCritRange       -> 198
    WeaponPadInt1         -> 199
    WeaponPadInt2         -> 200
    WeaponPadObj1         -> 201
    WeaponPadObj2         -> 202
    WeaponPadObj3         -> 203
    WeaponPadObj4         -> 204
    WeaponPadObj5         -> 205
    WeaponPadIntArr1      -> 206
    WeaponPadInt64Arr1    -> 207
    WeaponEnd             -> 208

  toEnum = \case
    187 -> WeaponBegin
    188 -> WeaponFlags
    189 -> WeaponRange
    190 -> WeaponAmmoType
    191 -> WeaponAmmoConsumption
    192 -> WeaponMissileAid
    193 -> WeaponCritHitChart
    194 -> WeaponAttacktype
    195 -> WeaponDamageDice
    196 -> WeaponAnimtype
    197 -> WeaponType
    198 -> WeaponCritRange
    199 -> WeaponPadInt1
    200 -> WeaponPadInt2
    201 -> WeaponPadObj1
    202 -> WeaponPadObj2
    203 -> WeaponPadObj3
    204 -> WeaponPadObj4
    205 -> WeaponPadObj5
    206 -> WeaponPadIntArr1
    207 -> WeaponPadInt64Arr1
    208 -> WeaponEnd
    n -> error $ "toEnum @WeaponField: bad value: " ++ show n

weaponFieldName :: WeaponField -> String
weaponFieldName = \case
  WeaponBegin           -> "obj_f_weapon_begin"
  WeaponFlags           -> "obj_f_weapon_flags"
  WeaponRange           -> "obj_f_weapon_range"
  WeaponAmmoType        -> "obj_f_weapon_ammo_type"
  WeaponAmmoConsumption -> "obj_f_weapon_ammo_consumption"
  WeaponMissileAid      -> "obj_f_weapon_missile_aid"
  WeaponCritHitChart    -> "obj_f_weapon_crit_hit_chart"
  WeaponAttacktype      -> "obj_f_weapon_attacktype"
  WeaponDamageDice      -> "obj_f_weapon_damage_dice"
  WeaponAnimtype        -> "obj_f_weapon_animtype"
  WeaponType            -> "obj_f_weapon_type"
  WeaponCritRange       -> "obj_f_weapon_crit_range"
  WeaponPadInt1         -> "obj_f_weapon_pad_i_1"
  WeaponPadInt2         -> "obj_f_weapon_pad_i_2"
  WeaponPadObj1         -> "obj_f_weapon_pad_obj_1"
  WeaponPadObj2         -> "obj_f_weapon_pad_obj_2"
  WeaponPadObj3         -> "obj_f_weapon_pad_obj_3"
  WeaponPadObj4         -> "obj_f_weapon_pad_obj_4"
  WeaponPadObj5         -> "obj_f_weapon_pad_obj_5"
  WeaponPadIntArr1      -> "obj_f_weapon_pad_ias_1"
  WeaponPadInt64Arr1    -> "obj_f_weapon_pad_i64as_1"
  WeaponEnd             -> "obj_f_weapon_end"

weaponFieldType :: WeaponField -> FieldType
weaponFieldType = \case
  WeaponBegin -> BeginF
  WeaponFlags -> W32F
  WeaponRange -> W32F
  WeaponAmmoType -> W32F
  WeaponAmmoConsumption -> W32F
  WeaponMissileAid -> W32F
  WeaponCritHitChart -> W32F
  WeaponAttacktype -> W32F
  WeaponDamageDice -> W32F
  WeaponAnimtype -> W32F
  WeaponType -> W32F
  WeaponCritRange -> W32F
  WeaponPadInt1 -> W32F
  WeaponPadInt2 -> W32F
  WeaponPadObj1 -> ObjF
  WeaponPadObj2 -> ObjF
  WeaponPadObj3 -> ObjF
  WeaponPadObj4 -> ObjF
  WeaponPadObj5 -> ObjF
  WeaponPadIntArr1 -> W32ArrF
  WeaponPadInt64Arr1 -> W64ArrF
  WeaponEnd -> EndF


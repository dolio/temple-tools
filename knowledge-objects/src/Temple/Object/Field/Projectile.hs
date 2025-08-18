
module Temple.Object.Field.Projectile where

import Temple.Object.Field.Type

data ProjectileField
  = ProjectileBegin
  | ProjectileFlagsCombat
  | ProjectileFlagsCombatDamage
  | ProjectileParentWeapon
  | ProjectileParentAmmo
  | ProjectilePartSysId
  | ProjectileAccelerationX
  | ProjectileAccelerationY
  | ProjectileAccelerationZ
  | ProjectilePadInt4
  | ProjectilePadObj1
  | ProjectilePadObj2
  | ProjectilePadObj3
  | ProjectilePadIntArr1
  | ProjectilePadInt64Arr1
  | ProjectilePadObjArr1
  | ProjectileEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum ProjectileField where
  fromEnum = \case
    ProjectileBegin             -> 134
    ProjectileFlagsCombat       -> 135
    ProjectileFlagsCombatDamage -> 136
    ProjectileParentWeapon      -> 137
    ProjectileParentAmmo        -> 138
    ProjectilePartSysId         -> 139
    ProjectileAccelerationX     -> 140
    ProjectileAccelerationY     -> 141
    ProjectileAccelerationZ     -> 142
    ProjectilePadInt4           -> 143
    ProjectilePadObj1           -> 144
    ProjectilePadObj2           -> 145
    ProjectilePadObj3           -> 146
    ProjectilePadIntArr1        -> 147
    ProjectilePadInt64Arr1      -> 148
    ProjectilePadObjArr1        -> 149
    ProjectileEnd               -> 150

  toEnum = \case
    134 -> ProjectileBegin
    135 -> ProjectileFlagsCombat
    136 -> ProjectileFlagsCombatDamage
    137 -> ProjectileParentWeapon
    138 -> ProjectileParentAmmo
    139 -> ProjectilePartSysId
    140 -> ProjectileAccelerationX
    141 -> ProjectileAccelerationY
    142 -> ProjectileAccelerationZ
    143 -> ProjectilePadInt4
    144 -> ProjectilePadObj1
    145 -> ProjectilePadObj2
    146 -> ProjectilePadObj3
    147 -> ProjectilePadIntArr1
    148 -> ProjectilePadInt64Arr1
    149 -> ProjectilePadObjArr1
    150 -> ProjectileEnd
    n -> error $ "toEnum @ProjectileField: bad value: " ++ show n

projectileFieldName :: ProjectileField -> String
projectileFieldName = \case
  ProjectileBegin             -> "obj_f_projectile_begin"
  ProjectileFlagsCombat       -> "obj_f_projectile_flags_combat"
  ProjectileFlagsCombatDamage -> "obj_f_projectile_flags_combat_damage"
  ProjectileParentWeapon      -> "obj_f_projectile_parent_weapon"
  ProjectileParentAmmo        -> "obj_f_projectile_parent_ammo"
  ProjectilePartSysId         -> "obj_f_projectile_part_sys_id"
  ProjectileAccelerationX     -> "obj_f_projectile_acceleration_x"
  ProjectileAccelerationY     -> "obj_f_projectile_acceleration_y"
  ProjectileAccelerationZ     -> "obj_f_projectile_acceleration_z"
  ProjectilePadInt4           -> "obj_f_projectile_pad_i_4"
  ProjectilePadObj1           -> "obj_f_projectile_pad_obj_1"
  ProjectilePadObj2           -> "obj_f_projectile_pad_obj_2"
  ProjectilePadObj3           -> "obj_f_projectile_pad_obj_3"
  ProjectilePadIntArr1        -> "obj_f_projectile_pad_ias_1"
  ProjectilePadInt64Arr1      -> "obj_f_projectile_pad_i64as_1"
  ProjectilePadObjArr1        -> "obj_f_projectile_pad_objas_1"
  ProjectileEnd               -> "obj_f_projectile_end"

projectileFieldType :: ProjectileField -> FieldType
projectileFieldType = \case
  ProjectileBegin -> BeginF
  ProjectileFlagsCombat -> W32F
  ProjectileFlagsCombatDamage -> W32F
  ProjectileParentWeapon -> ObjF
  ProjectileParentAmmo -> ObjF
  ProjectilePartSysId -> W32F
  ProjectileAccelerationX -> F32F
  ProjectileAccelerationY -> F32F
  ProjectileAccelerationZ -> F32F
  ProjectilePadInt4 -> W32F
  ProjectilePadObj1 -> ObjF
  ProjectilePadObj2 -> ObjF
  ProjectilePadObj3 -> ObjF
  ProjectilePadIntArr1 -> W32ArrF
  ProjectilePadInt64Arr1 -> W64ArrF
  ProjectilePadObjArr1 -> ObjArrF
  ProjectileEnd -> EndF



module Temple.Object.Field.General where

import Data.String
import Text.Megaparsec

import Temple.Object.Field.Type

-- General fields belong to every object
data GeneralField
  = GeneralBegin
  | Location
  | XOffset
  | YOffset
  | ShadowArt2D
  | BlitFlags
  | BlitColor
  | Transparency
  | ModelScale
  | LightFlags
  | LightMaterial
  | LightColor
  | LightRadius
  | LightAngleStart
  | LightAngleEnd
  | LightType
  | LightXFacing
  | LightYFacing
  | LightZFacing
  | LightXOffset
  | LightYOffset
  | LightZOffset
  | Flags
  | SpellFlags
  | Name
  | Description
  | Size
  -- Above here, the names may not be accurate
  | HpPts
  | HpAdj
  | HpDamage
  | Material
  | ScriptsIdx
  | SoundEffect
  | Category
  | Rotation
  | SpeedWalk
  | SpeedRun
  | BaseMesh
  | BaseAnim
  | Radius
  | RenderHeight3D
  | Conditions
  | ConditionArg0
  | PermanentMods
  | Initiative
  | Dispatcher
  | Subinitiative
  | SecretdoorFlags
  | SecretdoorEffectname
  | SecretdoorDC
  | PadInt7
  | PadInt8
  | PadInt9
  | PadInt0
  | ZOffset
  | RotationPitch
  | PadFloat3
  | PadFloat4
  | PadFloat5
  | PadFloat6
  | PadFloat7
  | PadFloat8
  | PadFloat9
  | PadFloat0
  | PadInt640
  | PadInt641
  | PadInt642
  | PadInt643
  | PadInt644
  | LastHitBy
  | PadObj1
  | PadObj2
  | PadObj3
  | PadObj4
  | PermanentModData
  | AttackTypesIdx
  | AttackBonusIdx
  | StrategyState
  | PadIntArr4
  | PadInt64Arr0
  | PadInt64Arr1
  | PadInt64Arr2
  | PadInt64Arr3
  | PadInt64Arr4
  | PadObjArr0
  | PadObjArr1
  | PadObjArr2
  | GeneralEnd
  deriving (Bounded, Enum, Eq, Ord, Show)

generalFieldName :: GeneralField -> String
generalFieldName = \case
  GeneralBegin -> "obj_f_begin"
  Location -> "obj_f_location"
  XOffset -> "obj_f_offset_x"
  YOffset -> "obj_f_offset_y"
  ShadowArt2D -> "obj_f_2D_shadow_art"
  BlitFlags -> "obj_f_blit_flags"
  BlitColor -> "obj_f_blit_color"
  Transparency -> "obj_f_transparency"
  ModelScale -> "obj_f_model_scale"
  LightFlags -> "obj_f_light_flags"
  LightMaterial -> "obj_f_light_material"
  LightColor -> "obj_f_light_color"
  LightRadius -> "obj_f_light_radius"
  LightAngleStart -> "obj_f_light_angle_start"
  LightAngleEnd -> "obj_f_light_angle_end"
  LightType -> "obj_f_light_type"
  LightXFacing -> "obj_f_light_facing_X"
  LightYFacing -> "obj_f_light_facing_Y"
  LightZFacing -> "obj_f_light_facing_Z"
  LightXOffset -> "obj_f_light_offset_X"
  LightYOffset -> "obj_f_light_offset_Y"
  LightZOffset -> "obj_f_light_offset_Z"
  Flags -> "obj_f_flags"
  SpellFlags -> "obj_f_spell_flags"
  Name -> "obj_f_name"
  Description -> "obj_f_description"
  Size -> "obj_f_size"
  HpPts -> "obj_f_hp_pts"
  HpAdj -> "obj_f_hp_adj"
  HpDamage -> "obj_f_hp_damage"
  Material -> "obj_f_material"
  ScriptsIdx -> "obj_f_scripts_idx"
  SoundEffect -> "obj_f_sound_effect"
  Category -> "obj_f_category"
  Rotation -> "obj_f_rotation"
  SpeedWalk -> "obj_f_speed_walk"
  SpeedRun -> "obj_f_speed_run"
  BaseMesh -> "obj_f_base_mesh"
  BaseAnim -> "obj_f_base_anim"
  Radius -> "obj_f_radius"
  RenderHeight3D -> "obj_f_3d_render_height"
  Conditions -> "obj_f_conditions"
  ConditionArg0 -> "obj_f_condition_arg0"
  PermanentMods -> "obj_f_permanent_mods"
  Initiative -> "obj_f_initiative"
  Dispatcher -> "obj_f_dispatcher"
  Subinitiative -> "obj_f_subinitiative"
  SecretdoorFlags -> "obj_f_secretdoor_flags"
  SecretdoorEffectname -> "obj_f_secretdoor_effectname"
  SecretdoorDC -> "obj_f_secretdoor_dc"
  PadInt7 -> "obj_f_pad_i_7"
  PadInt8 -> "obj_f_pad_i_8"
  PadInt9 -> "obj_f_pad_i_9"
  PadInt0 -> "obj_f_pad_i_0"
  ZOffset -> "obj_f_offset_z"
  RotationPitch -> "obj_f_rotation_pitch"
  PadFloat3 -> "obj_f_pad_f_3"
  PadFloat4 -> "obj_f_pad_f_4"
  PadFloat5 -> "obj_f_pad_f_5"
  PadFloat6 -> "obj_f_pad_f_6"
  PadFloat7 -> "obj_f_pad_f_7"
  PadFloat8 -> "obj_f_pad_f_8"
  PadFloat9 -> "obj_f_pad_f_9"
  PadFloat0 -> "obj_f_pad_f_0"
  PadInt640 -> "obj_f_pad_i64_0"
  PadInt641 -> "obj_f_pad_i64_1"
  PadInt642 -> "obj_f_pad_i64_2"
  PadInt643 -> "obj_f_pad_i64_3"
  PadInt644 -> "obj_f_pad_i64_4"
  LastHitBy -> "obj_f_last_hit_by"
  PadObj1 -> "obj_f_pad_obj_1"
  PadObj2 -> "obj_f_pad_obj_2"
  PadObj3 -> "obj_f_pad_obj_3"
  PadObj4 -> "obj_f_pad_obj_4"
  PermanentModData -> "obj_f_permanent_mod_data"
  AttackTypesIdx -> "obj_f_attack_types_idx"
  AttackBonusIdx -> "obj_f_attack_bonus_idx"
  StrategyState -> "obj_f_strategy_state"
  PadIntArr4 -> "obj_f_pad_ias_4"
  PadInt64Arr0 -> "obj_f_pad_i64as_0"
  PadInt64Arr1 -> "obj_f_pad_i64as_1"
  PadInt64Arr2 -> "obj_f_pad_i64as_2"
  PadInt64Arr3 -> "obj_f_pad_i64as_3"
  PadInt64Arr4 -> "obj_f_pad_i64as_4"
  PadObjArr0 -> "obj_f_pad_objas_0"
  PadObjArr1 -> "obj_f_pad_objas_1"
  PadObjArr2 -> "obj_f_pad_objas_2"
  GeneralEnd -> "obj_f_end"

generalFieldType :: GeneralField -> FieldType
generalFieldType = \case
  GeneralBegin -> BeginF
  Location -> LocF
  XOffset -> F32F
  YOffset -> F32F
  ShadowArt2D -> W32F
  BlitFlags -> W32F
  BlitColor -> W32F
  Transparency -> W32F
  ModelScale -> W32F
  LightFlags -> W32F
  LightMaterial -> W32F
  LightColor -> W32F
  LightRadius -> F32F
  LightAngleStart -> F32F
  LightAngleEnd -> F32F
  LightType -> W32F
  LightXFacing -> F32F
  LightYFacing -> F32F
  LightZFacing -> F32F
  LightXOffset -> F32F
  LightYOffset -> F32F
  LightZOffset -> F32F
  Flags -> W32F
  SpellFlags -> W32F
  Name -> W32F
  Description -> W32F
  Size -> W32F
  HpPts -> W32F
  HpAdj -> W32F
  HpDamage -> W32F
  Material -> W32F
  ScriptsIdx -> ScriptArrF
  SoundEffect -> W32F
  Category -> W32F
  Rotation -> F32F
  SpeedWalk -> F32F
  SpeedRun -> F32F
  BaseMesh -> W32F
  BaseAnim -> W32F
  Radius -> F32F
  RenderHeight3D -> F32F
  Conditions -> CondArrF
  ConditionArg0 -> W32ArrF
  PermanentMods -> CondArrF
  Initiative -> W32F
  Dispatcher -> B32F
  Subinitiative -> W32F
  SecretdoorFlags -> W32F
  SecretdoorEffectname -> W32F
  SecretdoorDC -> W32F
  PadInt7 -> W32F
  PadInt8 -> W32F
  PadInt9 -> W32F
  PadInt0 -> W32F
  ZOffset -> F32F
  RotationPitch -> F32F
  PadFloat3 -> F32F
  PadFloat4 -> F32F
  PadFloat5 -> F32F
  PadFloat6 -> F32F
  PadFloat7 -> F32F
  PadFloat8 -> F32F
  PadFloat9 -> F32F
  PadFloat0 -> F32F
  PadInt640 -> W64F
  PadInt641 -> W64F
  PadInt642 -> W64F
  PadInt643 -> W64F
  PadInt644 -> W64F
  LastHitBy -> ObjF
  PadObj1 -> ObjF
  PadObj2 -> ObjF
  PadObj3 -> ObjF
  PadObj4 -> ObjF
  PermanentModData -> W32ArrF
  AttackTypesIdx -> W32ArrF
  AttackBonusIdx -> W32ArrF
  StrategyState -> W32ArrF
  PadIntArr4 -> W32ArrF
  PadInt64Arr0 -> W64ArrF
  PadInt64Arr1 -> W64ArrF
  PadInt64Arr2 -> W64ArrF
  PadInt64Arr3 -> W64ArrF
  PadInt64Arr4 -> W64ArrF
  PadObjArr0 -> ObjArrF
  PadObjArr1 -> ObjArrF
  PadObjArr2 -> ObjArrF
  GeneralEnd -> EndF

parsePartialGeneralFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m GeneralField
parsePartialGeneralFieldName =
  choice
    [ GeneralBegin <$ chunk "begin"
    , Location <$ chunk "location"
    , XOffset <$ chunk "offset_x"
    , YOffset <$ chunk "offset_y"
    , ShadowArt2D <$ chunk "2D_shadow_art"
    , BlitFlags <$ chunk "blit_flags"
    , BlitColor <$ chunk "blit_color"
    , Transparency <$ chunk "transparency"
    , ModelScale <$ chunk "model_scale"
    , LightFlags <$ chunk "light_flags"
    , LightMaterial <$ chunk "light_material"
    , LightColor <$ chunk "light_color"
    , LightRadius <$ chunk "light_radius"
    , LightAngleStart <$ chunk "light_angle_start"
    , LightAngleEnd <$ chunk "light_angle_end"
    , LightType <$ chunk "light_type"
    , LightXFacing <$ chunk "light_facing_X"
    , LightYFacing <$ chunk "light_facing_Y"
    , LightZFacing <$ chunk "light_facing_Z"
    , LightXOffset <$ chunk "light_offset_X"
    , LightYOffset <$ chunk "light_offset_Y"
    , LightZOffset <$ chunk "light_offset_Z"
    , Flags <$ chunk "flags"
    , SpellFlags <$ chunk "spell_flags"
    , Name <$ chunk "name"
    , Description <$ chunk "description"
    , Size <$ chunk "size"
    , HpPts <$ chunk "hp_pts"
    , HpAdj <$ chunk "hp_adj"
    , HpDamage <$ chunk "hp_damage"
    , Material <$ chunk "material"
    , ScriptsIdx <$ chunk "scripts_idx"
    , SoundEffect <$ chunk "sound_effect"
    , Category <$ chunk "category"
    , Rotation <$ chunk "rotation"
    , SpeedWalk <$ chunk "speed_walk"
    , SpeedRun <$ chunk "speed_run"
    , BaseMesh <$ chunk "base_mesh"
    , BaseAnim <$ chunk "base_anim"
    , Radius <$ chunk "radius"
    , RenderHeight3D <$ chunk "3d_render_height"
    , Conditions <$ chunk "conditions"
    , ConditionArg0 <$ chunk "condition_arg0"
    , PermanentMods <$ chunk "permanent_mods"
    , Initiative <$ chunk "initiative"
    , Dispatcher <$ chunk "dispatcher"
    , Subinitiative <$ chunk "subinitiative"
    , SecretdoorFlags <$ chunk "secretdoor_flags"
    , SecretdoorEffectname <$ chunk "secretdoor_effectname"
    , SecretdoorDC <$ chunk "secretdoor_dc"
    , PadInt7 <$ chunk "pad_i_7"
    , PadInt8 <$ chunk "pad_i_8"
    , PadInt9 <$ chunk "pad_i_9"
    , PadInt0 <$ chunk "pad_i_0"
    , ZOffset <$ chunk "offset_z"
    , RotationPitch <$ chunk "rotation_pitch"
    , PadFloat3 <$ chunk "pad_f_3"
    , PadFloat4 <$ chunk "pad_f_4"
    , PadFloat5 <$ chunk "pad_f_5"
    , PadFloat6 <$ chunk "pad_f_6"
    , PadFloat7 <$ chunk "pad_f_7"
    , PadFloat8 <$ chunk "pad_f_8"
    , PadFloat9 <$ chunk "pad_f_9"
    , PadFloat0 <$ chunk "pad_f_0"
    , PadInt640 <$ chunk "pad_i64_0"
    , PadInt641 <$ chunk "pad_i64_1"
    , PadInt642 <$ chunk "pad_i64_2"
    , PadInt643 <$ chunk "pad_i64_3"
    , PadInt644 <$ chunk "pad_i64_4"
    , LastHitBy <$ chunk "last_hit_by"
    , PadObj1 <$ chunk "pad_obj_1"
    , PadObj2 <$ chunk "pad_obj_2"
    , PadObj3 <$ chunk "pad_obj_3"
    , PadObj4 <$ chunk "pad_obj_4"
    , PermanentModData <$ chunk "permanent_mod_data"
    , AttackTypesIdx <$ chunk "attack_types_idx"
    , AttackBonusIdx <$ chunk "attack_bonus_idx"
    , StrategyState <$ chunk "strategy_state"
    , PadIntArr4 <$ chunk "pad_ias_4"
    , PadInt64Arr0 <$ chunk "pad_i64as_0"
    , PadInt64Arr1 <$ chunk "pad_i64as_1"
    , PadInt64Arr2 <$ chunk "pad_i64as_2"
    , PadInt64Arr3 <$ chunk "pad_i64as_3"
    , PadInt64Arr4 <$ chunk "pad_i64as_4"
    , PadObjArr0 <$ chunk "pad_objas_0"
    , PadObjArr1 <$ chunk "pad_objas_1"
    , PadObjArr2 <$ chunk "pad_objas_2"
    , GeneralEnd <$ chunk "end"
    ]

parseGeneralFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m GeneralField
parseGeneralFieldName = chunk "obj_t_" *> parsePartialGeneralFieldName

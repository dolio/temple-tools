
module Temple.Object.Field.Item where

import Temple.Object.Field.Class
import Temple.Object.Field.Type

-- Item fields are common to inventory items.
data ItemField
  = ItemBegin
  | ItemFlags
  | ItemParent
  | ItemWeight
  | ItemWorth
  | ItemInvAid
  | ItemInvLocation
  | ItemGroundMesh
  | ItemGroundAnim
  | ItemDescriptionUnknown
  | ItemDescriptionEffects
  | ItemSpellIdx
  | ItemSpellIdxFlags
  | ItemSpellChargesIdx
  | ItemAiAction
  | ItemWearFlags
  | ItemMaterialSlot
  | ItemQuantity
  | ItemPadInt1
  | ItemPadInt2
  | ItemPadInt3
  | ItemPadInt4
  | ItemPadInt5
  | ItemPadInt6
  | ItemPadObj1
  | ItemPadObj2
  | ItemPadObj3
  | ItemPadObj4
  | ItemPadObj5
  | ItemPadWielderConditionArray
  | ItemPadWielderArgumentArray
  | ItemPadInt64Arr1
  | ItemPadInt64Arr2
  | ItemPadObjArr1
  | ItemPadObjArr2
  | ItemEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum ItemField where
  fromEnum = \case
    ItemBegin                    -> 151
    ItemFlags                    -> 152
    ItemParent                   -> 153
    ItemWeight                   -> 154
    ItemWorth                    -> 155
    ItemInvAid                   -> 156
    ItemInvLocation              -> 157
    ItemGroundMesh               -> 158
    ItemGroundAnim               -> 159
    ItemDescriptionUnknown       -> 160
    ItemDescriptionEffects       -> 161
    ItemSpellIdx                 -> 162
    ItemSpellIdxFlags            -> 163
    ItemSpellChargesIdx          -> 164
    ItemAiAction                 -> 165
    ItemWearFlags                -> 166
    ItemMaterialSlot             -> 167
    ItemQuantity                 -> 168
    ItemPadInt1                  -> 169
    ItemPadInt2                  -> 170
    ItemPadInt3                  -> 171
    ItemPadInt4                  -> 172
    ItemPadInt5                  -> 173
    ItemPadInt6                  -> 174
    ItemPadObj1                  -> 175
    ItemPadObj2                  -> 176
    ItemPadObj3                  -> 177
    ItemPadObj4                  -> 178
    ItemPadObj5                  -> 179
    ItemPadWielderConditionArray -> 180
    ItemPadWielderArgumentArray  -> 181
    ItemPadInt64Arr1             -> 182
    ItemPadInt64Arr2             -> 183
    ItemPadObjArr1               -> 184
    ItemPadObjArr2               -> 185
    ItemEnd                      -> 186

  toEnum = \case
    151 -> ItemBegin
    152 -> ItemFlags
    153 -> ItemParent
    154 -> ItemWeight
    155 -> ItemWorth
    156 -> ItemInvAid
    157 -> ItemInvLocation
    158 -> ItemGroundMesh
    159 -> ItemGroundAnim
    160 -> ItemDescriptionUnknown
    161 -> ItemDescriptionEffects
    162 -> ItemSpellIdx
    163 -> ItemSpellIdxFlags
    164 -> ItemSpellChargesIdx
    165 -> ItemAiAction
    166 -> ItemWearFlags
    167 -> ItemMaterialSlot
    168 -> ItemQuantity
    169 -> ItemPadInt1
    170 -> ItemPadInt2
    171 -> ItemPadInt3
    172 -> ItemPadInt4
    173 -> ItemPadInt5
    174 -> ItemPadInt6
    175 -> ItemPadObj1
    176 -> ItemPadObj2
    177 -> ItemPadObj3
    178 -> ItemPadObj4
    179 -> ItemPadObj5
    180 -> ItemPadWielderConditionArray
    181 -> ItemPadWielderArgumentArray
    182 -> ItemPadInt64Arr1
    183 -> ItemPadInt64Arr2
    184 -> ItemPadObjArr1
    185 -> ItemPadObjArr2
    186 -> ItemEnd
    n -> error $ "toEnum @ItemField: bad value: " ++ show n

  enumFrom n = enumFromTo n maxBound
  enumFromThen m n = enumFromThenTo m n maxBound

instance Field ItemField where
  fieldName = \case
    ItemBegin                    -> "obj_f_item_begin"
    ItemFlags                    -> "obj_f_item_flags"
    ItemParent                   -> "obj_f_item_parent"
    ItemWeight                   -> "obj_f_item_weight"
    ItemWorth                    -> "obj_f_item_worth"
    ItemInvAid                   -> "obj_f_item_inv_aid"
    ItemInvLocation              -> "obj_f_item_inv_location"
    ItemGroundMesh               -> "obj_f_item_ground_mesh"
    ItemGroundAnim               -> "obj_f_item_ground_anim"
    ItemDescriptionUnknown       -> "obj_f_item_description_unknown"
    ItemDescriptionEffects       -> "obj_f_item_description_effects"
    ItemSpellIdx                 -> "obj_f_item_spell_idx"
    ItemSpellIdxFlags            -> "obj_f_item_spell_idx_flags"
    ItemSpellChargesIdx          -> "obj_f_item_spell_charges_idx"
    ItemAiAction                 -> "obj_f_item_ai_action"
    ItemWearFlags                -> "obj_f_item_wear_flags"
    ItemMaterialSlot             -> "obj_f_item_material_slot"
    ItemQuantity                 -> "obj_f_item_quantity"
    ItemPadInt1                  -> "obj_f_item_pad_i_1"
    ItemPadInt2                  -> "obj_f_item_pad_i_2"
    ItemPadInt3                  -> "obj_f_item_pad_i_3"
    ItemPadInt4                  -> "obj_f_item_pad_i_4"
    ItemPadInt5                  -> "obj_f_item_pad_i_5"
    ItemPadInt6                  -> "obj_f_item_pad_i_6"
    ItemPadObj1                  -> "obj_f_item_pad_obj_1"
    ItemPadObj2                  -> "obj_f_item_pad_obj_2"
    ItemPadObj3                  -> "obj_f_item_pad_obj_3"
    ItemPadObj4                  -> "obj_f_item_pad_obj_4"
    ItemPadObj5                  -> "obj_f_item_pad_obj_5"
    ItemPadWielderConditionArray -> "obj_f_item_pad_wielder_condition_array"
    ItemPadWielderArgumentArray  -> "obj_f_item_pad_wielder_argument_array"
    ItemPadInt64Arr1             -> "obj_f_item_pad_i64as_1"
    ItemPadInt64Arr2             -> "obj_f_item_pad_i64as_2"
    ItemPadObjArr1               -> "obj_f_item_pad_objas_1"
    ItemPadObjArr2               -> "obj_f_item_pad_objas_2"
    ItemEnd                      -> "obj_f_item_end"

  fieldType = \case
    ItemBegin -> BeginF
    ItemFlags -> W32F
    ItemParent -> ObjF
    ItemWeight -> W32F
    ItemWorth -> W32F
    ItemInvAid -> W32F
    ItemInvLocation -> W32F
    ItemGroundMesh -> W32F
    ItemGroundAnim -> W32F
    ItemDescriptionUnknown -> W32F
    ItemDescriptionEffects -> W32F
    ItemSpellIdx -> SpellArrF
    ItemSpellIdxFlags -> W32F
    ItemSpellChargesIdx -> W32F
    ItemAiAction -> W32F
    ItemWearFlags -> W32F
    ItemMaterialSlot -> W32F
    ItemQuantity -> W32F
    ItemPadInt1 -> W32F
    ItemPadInt2 -> W32F
    ItemPadInt3 -> W32F
    ItemPadInt4 -> W32F
    ItemPadInt5 -> W32F
    ItemPadInt6 -> W32F
    ItemPadObj1 -> ObjF
    ItemPadObj2 -> ObjF
    ItemPadObj3 -> ObjF
    ItemPadObj4 -> ObjF
    ItemPadObj5 -> ObjF
    ItemPadWielderConditionArray -> W32ArrF
    ItemPadWielderArgumentArray -> W32ArrF
    ItemPadInt64Arr1 -> W64ArrF
    ItemPadInt64Arr2 -> W64ArrF
    ItemPadObjArr1 -> ObjArrF
    ItemPadObjArr2 -> ObjArrF
    ItemEnd -> EndF

  isPadding = \case
    ItemPadInt1 -> True
    ItemPadInt2 -> True
    ItemPadInt3 -> True
    ItemPadInt4 -> True
    ItemPadInt5 -> True
    ItemPadInt6 -> True
    ItemPadObj1 -> True
    ItemPadObj2 -> True
    ItemPadObj3 -> True
    ItemPadObj4 -> True
    ItemPadObj5 -> True
    ItemPadWielderConditionArray -> True
    ItemPadWielderArgumentArray -> True
    ItemPadInt64Arr1 -> True
    ItemPadInt64Arr2 -> True
    ItemPadObjArr1 -> True
    ItemPadObjArr2 -> True
    _ -> False


module Temple.Object.Field.Portal where

import Temple.Object.Field.Type

data PortalField
  = PortalBegin
  | PortalFlags
  | PortalLockDC
  | PortalKeyId
  | PortalNotifyNpc
  | PortalPadInt1
  | PortalPadInt2
  | PortalPadInt3
  | PortalPadInt4
  | PortalPadInt5
  | PortalPadObj1
  | PortalPadIntArr1
  | PortalPadInt64Arr1
  | PortalEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum PortalField where
  fromEnum = \case
    PortalBegin -> 88
    PortalFlags -> 89
    PortalLockDC -> 90
    PortalKeyId -> 91
    PortalNotifyNpc -> 92
    PortalPadInt1 -> 93
    PortalPadInt2 -> 94
    PortalPadInt3 -> 95
    PortalPadInt4 -> 96
    PortalPadInt5 -> 97
    PortalPadObj1 -> 98
    PortalPadIntArr1 -> 99
    PortalPadInt64Arr1 -> 100
    PortalEnd -> 101

  toEnum = \case
    88 -> PortalBegin
    89 -> PortalFlags
    90 -> PortalLockDC
    91 -> PortalKeyId
    92 -> PortalNotifyNpc
    93 -> PortalPadInt1
    94 -> PortalPadInt2
    95 -> PortalPadInt3
    96 -> PortalPadInt4
    97 -> PortalPadInt5
    98 -> PortalPadObj1
    99 -> PortalPadIntArr1
    100 -> PortalPadInt64Arr1
    101 -> PortalEnd
    n -> error $ "toEnum @PortalField: bad value: " ++ show n

portalFieldName :: PortalField -> String
portalFieldName = \case
  PortalBegin        -> "obj_f_portal_begin"
  PortalFlags        -> "obj_f_portal_flags"
  PortalLockDC       -> "obj_f_portal_lock_dc"
  PortalKeyId        -> "obj_f_portal_key_id"
  PortalNotifyNpc    -> "obj_f_portal_notify_npc"
  PortalPadInt1      -> "obj_f_portal_pad_i_1"
  PortalPadInt2      -> "obj_f_portal_pad_i_2"
  PortalPadInt3      -> "obj_f_portal_pad_i_3"
  PortalPadInt4      -> "obj_f_portal_pad_i_4"
  PortalPadInt5      -> "obj_f_portal_pad_i_5"
  PortalPadObj1      -> "obj_f_portal_pad_obj_1"
  PortalPadIntArr1   -> "obj_f_portal_pad_ias_1"
  PortalPadInt64Arr1 -> "obj_f_portal_pad_i64as_1"
  PortalEnd          -> "obj_f_portal_end"

portalFieldType :: PortalField -> FieldType
portalFieldType = \case
  PortalBegin -> BeginF
  PortalFlags -> W32F
  PortalLockDC -> W32F
  PortalKeyId -> W32F
  PortalNotifyNpc -> W32F
  PortalPadInt1 -> W32F
  PortalPadInt2 -> W32F
  PortalPadInt3 -> W32F
  PortalPadInt4 -> W32F
  PortalPadInt5 -> W32F
  PortalPadObj1 -> ObjF
  PortalPadIntArr1 -> W32ArrF
  PortalPadInt64Arr1 -> W64ArrF
  PortalEnd -> EndF



module Temple.Object.Field.Container where

import Data.String
import Text.Megaparsec

import Temple.Object.Field.Type

data ContainerField
  = ContainerBegin
  | ContainerFlags
  | ContainerLockDC
  | ContainerKeyId
  | ContainerInventoryNum
  | ContainerInventoryListIdx
  | ContainerInventorySource
  | ContainerNotifyNpc
  | ContainerPadInt1
  | ContainerPadInt2
  | ContainerPadInt3
  | ContainerPadInt4
  | ContainerPadInt5
  | ContainerPadObj1
  | ContainerPadObj2
  | ContainerPadIntArr1
  | ContainerPadInt64Arr1
  | ContainerPadObjArr1
  | ContainerEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum ContainerField where
  fromEnum = \case
    ContainerBegin            -> 102
    ContainerFlags            -> 103
    ContainerLockDC           -> 104
    ContainerKeyId            -> 105
    ContainerInventoryNum     -> 106
    ContainerInventoryListIdx -> 107
    ContainerInventorySource  -> 108
    ContainerNotifyNpc        -> 109
    ContainerPadInt1          -> 110
    ContainerPadInt2          -> 111
    ContainerPadInt3          -> 112
    ContainerPadInt4          -> 113
    ContainerPadInt5          -> 114
    ContainerPadObj1          -> 115
    ContainerPadObj2          -> 116
    ContainerPadIntArr1       -> 117
    ContainerPadInt64Arr1     -> 118
    ContainerPadObjArr1       -> 119
    ContainerEnd              -> 120

  toEnum = \case
    102 -> ContainerBegin
    103 -> ContainerFlags
    104 -> ContainerLockDC
    105 -> ContainerKeyId
    106 -> ContainerInventoryNum
    107 -> ContainerInventoryListIdx
    108 -> ContainerInventorySource
    109 -> ContainerNotifyNpc
    110 -> ContainerPadInt1
    111 -> ContainerPadInt2
    112 -> ContainerPadInt3
    113 -> ContainerPadInt4
    114 -> ContainerPadInt5
    115 -> ContainerPadObj1
    116 -> ContainerPadObj2
    117 -> ContainerPadIntArr1
    118 -> ContainerPadInt64Arr1
    119 -> ContainerPadObjArr1
    120 -> ContainerEnd
    n -> error $ "toEnum @ContainerField: bad value: " ++ show n

containerFieldName :: ContainerField -> String
containerFieldName = \case
  ContainerBegin            -> "obj_f_container_begin"
  ContainerFlags            -> "obj_f_container_flags"
  ContainerLockDC           -> "obj_f_container_lock_dc"
  ContainerKeyId            -> "obj_f_container_key_id"
  ContainerInventoryNum     -> "obj_f_container_inventory_num"
  ContainerInventoryListIdx -> "obj_f_container_inventory_list_idx"
  ContainerInventorySource  -> "obj_f_container_inventory_source"
  ContainerNotifyNpc        -> "obj_f_container_notify_npc"
  ContainerPadInt1          -> "obj_f_container_pad_i_1"
  ContainerPadInt2          -> "obj_f_container_pad_i_2"
  ContainerPadInt3          -> "obj_f_container_pad_i_3"
  ContainerPadInt4          -> "obj_f_container_pad_i_4"
  ContainerPadInt5          -> "obj_f_container_pad_i_5"
  ContainerPadObj1          -> "obj_f_container_pad_obj_1"
  ContainerPadObj2          -> "obj_f_container_pad_obj_2"
  ContainerPadIntArr1       -> "obj_f_container_pad_ias_1"
  ContainerPadInt64Arr1     -> "obj_f_container_pad_i64as_1"
  ContainerPadObjArr1       -> "obj_f_container_pad_objas_1"
  ContainerEnd              -> "obj_f_container_end"

containerFieldType :: ContainerField -> FieldType
containerFieldType = \case
  ContainerBegin -> BeginF
  ContainerFlags -> W32F
  ContainerLockDC -> W32F
  ContainerKeyId -> W32F
  ContainerInventoryNum -> W32F
  ContainerInventoryListIdx -> ObjArrF
  ContainerInventorySource -> W32F
  ContainerNotifyNpc -> W32F
  ContainerPadInt1 -> W32F
  ContainerPadInt2 -> W32F
  ContainerPadInt3 -> W32F
  ContainerPadInt4 -> W32F
  ContainerPadInt5 -> W32F
  ContainerPadObj1 -> ObjF
  ContainerPadObj2 -> ObjF
  ContainerPadIntArr1 -> W32ArrF
  ContainerPadInt64Arr1 -> W64ArrF
  ContainerPadObjArr1 -> ObjArrF
  ContainerEnd -> EndF

parsePartialContainerFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m ContainerField
parsePartialContainerFieldName =
  choice
    [ ContainerBegin            <$ chunk "begin"
    , ContainerFlags            <$ chunk "flags"
    , ContainerLockDC           <$ chunk "lock_dc"
    , ContainerKeyId            <$ chunk "key_id"
    , ContainerInventoryNum     <$ chunk "inventory_num"
    , ContainerInventoryListIdx <$ chunk "inventory_list_idx"
    , ContainerInventorySource  <$ chunk "inventory_source"
    , ContainerNotifyNpc        <$ chunk "notify_npc"
    , ContainerPadInt1          <$ chunk "pad_i_1"
    , ContainerPadInt2          <$ chunk "pad_i_2"
    , ContainerPadInt3          <$ chunk "pad_i_3"
    , ContainerPadInt4          <$ chunk "pad_i_4"
    , ContainerPadInt5          <$ chunk "pad_i_5"
    , ContainerPadObj1          <$ chunk "pad_obj_1"
    , ContainerPadObj2          <$ chunk "pad_obj_2"
    , ContainerPadIntArr1       <$ chunk "pad_ias_1"
    , ContainerPadInt64Arr1     <$ chunk "pad_i64as_1"
    , ContainerPadObjArr1       <$ chunk "pad_objas_1"
    , ContainerEnd              <$ chunk "end"
    ]

parseContainerFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m ContainerField
parseContainerFieldName =
  chunk "obj_f_container_" *> parsePartialContainerFieldName

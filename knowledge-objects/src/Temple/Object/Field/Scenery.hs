
module Temple.Object.Field.Scenery where

import Data.String
import Text.Megaparsec

import Temple.Object.Field.Type

data SceneryField
  = SceneryBegin
  | SceneryFlags
  | SceneryPadObj0
  | SceneryRespawnDelay
  | SceneryPadInt0
  | SceneryPadInt1
  | SceneryTeleportTo
  | SceneryPadInt4
  | SceneryPadInt5
  | SceneryPadObj1
  | SceneryPadIntArr1
  | SceneryPadInt64Arr1
  | SceneryEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum SceneryField where
  fromEnum = \case
    SceneryBegin        -> 121
    SceneryFlags        -> 122
    SceneryPadObj0      -> 123
    SceneryRespawnDelay -> 124
    SceneryPadInt0      -> 125
    SceneryPadInt1      -> 126
    SceneryTeleportTo   -> 127
    SceneryPadInt4      -> 128
    SceneryPadInt5      -> 129
    SceneryPadObj1      -> 130
    SceneryPadIntArr1   -> 131
    SceneryPadInt64Arr1 -> 132
    SceneryEnd          -> 133

  toEnum = \case
    121 -> SceneryBegin
    122 -> SceneryFlags
    123 -> SceneryPadObj0
    124 -> SceneryRespawnDelay
    125 -> SceneryPadInt0
    126 -> SceneryPadInt1
    127 -> SceneryTeleportTo
    128 -> SceneryPadInt4
    129 -> SceneryPadInt5
    130 -> SceneryPadObj1
    131 -> SceneryPadIntArr1
    132 -> SceneryPadInt64Arr1
    133 -> SceneryEnd
    n -> error $ "toEnum @SceneryField: bad value: " ++ show n

sceneryFieldName :: SceneryField -> String
sceneryFieldName = \case
  SceneryBegin        -> "obj_f_scenery_begin"
  SceneryFlags        -> "obj_f_scenery_flags"
  SceneryPadObj0      -> "obj_f_scenery_pad_obj_0"
  SceneryRespawnDelay -> "obj_f_scenery_respawn_delay"
  SceneryPadInt0      -> "obj_f_scenery_pad_i_0"
  SceneryPadInt1      -> "obj_f_scenery_pad_i_1"
  SceneryTeleportTo   -> "obj_f_scenery_teleport_to"
  SceneryPadInt4      -> "obj_f_scenery_pad_i_4"
  SceneryPadInt5      -> "obj_f_scenery_pad_i_5"
  SceneryPadObj1      -> "obj_f_scenery_pad_obj_1"
  SceneryPadIntArr1   -> "obj_f_scenery_pad_ias_1"
  SceneryPadInt64Arr1 -> "obj_f_scenery_pad_i64as_1"
  SceneryEnd          -> "obj_f_scenery_end"

sceneryFieldType :: SceneryField -> FieldType
sceneryFieldType = \case
  SceneryBegin -> BeginF
  SceneryFlags -> W32F
  SceneryPadObj0 -> ObjF
  SceneryRespawnDelay -> W32F
  SceneryPadInt0 -> W32F
  SceneryPadInt1 -> W32F
  SceneryTeleportTo -> W32F
  SceneryPadInt4 -> W32F
  SceneryPadInt5 -> W32F
  SceneryPadObj1 -> W32F
  SceneryPadIntArr1 -> W32ArrF
  SceneryPadInt64Arr1 -> W64ArrF
  SceneryEnd -> EndF

parsePartialSceneryFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m SceneryField
parsePartialSceneryFieldName =
  choice
    [ SceneryBegin        <$ chunk "begin"
    , SceneryFlags        <$ chunk "flags"
    , SceneryPadObj0      <$ chunk "pad_obj_0"
    , SceneryRespawnDelay <$ chunk "respawn_delay"
    , SceneryPadInt0      <$ chunk "pad_i_0"
    , SceneryPadInt1      <$ chunk "pad_i_1"
    , SceneryTeleportTo   <$ chunk "teleport_to"
    , SceneryPadInt4      <$ chunk "pad_i_4"
    , SceneryPadInt5      <$ chunk "pad_i_5"
    , SceneryPadObj1      <$ chunk "pad_obj_1"
    , SceneryPadIntArr1   <$ chunk "pad_ias_1"
    , SceneryPadInt64Arr1 <$ chunk "pad_i64as_1"
    , SceneryEnd          <$ chunk "end"
    ]

parseSceneryFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m SceneryField
parseSceneryFieldName = chunk "obj_f_scenery_" *> parsePartialSceneryFieldName

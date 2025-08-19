
module Temple.Object.Field.Pc where

import Data.String
import Text.Megaparsec

import Temple.Object.Field.Type

data PcField
  = PcBegin
  | PcFlags
  | PcPadIntArr0
  | PcPadInt64Arr0
  | PcPlayerName
  | PcGlobalFlags
  | PcGlobalVariables
  | PcVoiceIdx
  | PcRollCount
  | PcPadInt2
  | PcWeaponslotsIdx
  | PcPadIntArr2
  | PcPadInt64Arr1
  | PcEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum PcField where
  fromEnum = \case
    PcBegin           -> 339
    PcFlags           -> 340
    PcPadIntArr0      -> 341
    PcPadInt64Arr0    -> 342
    PcPlayerName      -> 343
    PcGlobalFlags     -> 344
    PcGlobalVariables -> 345
    PcVoiceIdx        -> 346
    PcRollCount       -> 347
    PcPadInt2         -> 348
    PcWeaponslotsIdx  -> 349
    PcPadIntArr2      -> 350
    PcPadInt64Arr1    -> 351
    PcEnd             -> 352

  toEnum = \case
    339 -> PcBegin
    340 -> PcFlags
    341 -> PcPadIntArr0
    342 -> PcPadInt64Arr0
    343 -> PcPlayerName
    344 -> PcGlobalFlags
    345 -> PcGlobalVariables
    346 -> PcVoiceIdx
    347 -> PcRollCount
    348 -> PcPadInt2
    349 -> PcWeaponslotsIdx
    350 -> PcPadIntArr2
    351 -> PcPadInt64Arr1
    352 -> PcEnd
    n -> error $ "toEnum @PcField: bad value: " ++ show n

pcFieldName :: PcField -> String
pcFieldName = \case
  PcBegin           -> "obj_f_pc_begin"
  PcFlags           -> "obj_f_pc_flags"
  PcPadIntArr0      -> "obj_f_pc_pad_ias_0"
  PcPadInt64Arr0    -> "obj_f_pc_pad_i64as_0"
  PcPlayerName      -> "obj_f_pc_player_name"
  PcGlobalFlags     -> "obj_f_pc_global_flags"
  PcGlobalVariables -> "obj_f_pc_global_variables"
  PcVoiceIdx        -> "obj_f_pc_voice_idx"
  PcRollCount       -> "obj_f_pc_roll_count"
  PcPadInt2         -> "obj_f_pc_pad_i_2"
  PcWeaponslotsIdx  -> "obj_f_pc_weaponslots_idx"
  PcPadIntArr2      -> "obj_f_pc_pad_ias_2"
  PcPadInt64Arr1    -> "obj_f_pc_pad_i64as_1"
  PcEnd             -> "obj_f_pc_end"

pcFieldType :: PcField -> FieldType
pcFieldType = \case
  PcBegin -> BeginF
  PcFlags -> W32F
  PcPadIntArr0 -> W32ArrF
  PcPadInt64Arr0 -> W64ArrF
  PcPlayerName -> StringF
  PcGlobalFlags -> W32ArrF
  PcGlobalVariables -> W32ArrF
  PcVoiceIdx -> W32F
  PcRollCount -> W32F
  PcPadInt2 -> W32F
  PcWeaponslotsIdx -> W32ArrF
  PcPadIntArr2 -> W32ArrF
  PcPadInt64Arr1 -> W64ArrF
  PcEnd -> EndF

parsePartialPcFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m PcField
parsePartialPcFieldName =
  choice
    [ PcBegin           <$ chunk "begin"
    , PcFlags           <$ chunk "flags"
    , PcPadIntArr0      <$ chunk "pad_ias_0"
    , PcPadInt64Arr0    <$ chunk "pad_i64as_0"
    , PcPlayerName      <$ chunk "player_name"
    , PcGlobalFlags     <$ chunk "global_flags"
    , PcGlobalVariables <$ chunk "global_variables"
    , PcVoiceIdx        <$ chunk "voice_idx"
    , PcRollCount       <$ chunk "roll_count"
    , PcPadInt2         <$ chunk "pad_i_2"
    , PcWeaponslotsIdx  <$ chunk "weaponslots_idx"
    , PcPadIntArr2      <$ chunk "pad_ias_2"
    , PcPadInt64Arr1    <$ chunk "pad_i64as_1"
    , PcEnd             <$ chunk "end"
    ]

parsePcFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m PcField
parsePcFieldName = chunk "obj_f_pc_" *> parsePartialPcFieldName

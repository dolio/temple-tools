
module Temple.Object.Field.Money where

import Data.String
import Text.Megaparsec

import Temple.Object.Field.Type

data MoneyField
  = MoneyBegin
  | MoneyFlags
  | MoneyQuantity
  | MoneyType
  | MoneyPadInt1
  | MoneyPadInt2
  | MoneyPadInt3
  | MoneyPadInt4
  | MoneyPadInt5
  | MoneyPadIntArr1
  | MoneyPadInt64Arr1
  | MoneyEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum MoneyField where
  fromEnum = \case
    MoneyBegin        -> 229
    MoneyFlags        -> 230
    MoneyQuantity     -> 231
    MoneyType         -> 232
    MoneyPadInt1      -> 233
    MoneyPadInt2      -> 234
    MoneyPadInt3      -> 235
    MoneyPadInt4      -> 236
    MoneyPadInt5      -> 237
    MoneyPadIntArr1   -> 238
    MoneyPadInt64Arr1 -> 239
    MoneyEnd          -> 240

  toEnum = \case
    229 -> MoneyBegin
    230 -> MoneyFlags
    231 -> MoneyQuantity
    232 -> MoneyType
    233 -> MoneyPadInt1
    234 -> MoneyPadInt2
    235 -> MoneyPadInt3
    236 -> MoneyPadInt4
    237 -> MoneyPadInt5
    238 -> MoneyPadIntArr1
    239 -> MoneyPadInt64Arr1
    240 -> MoneyEnd
    n -> error $ "toEnum @MoneyField: bad value: " ++ show n

moneyFieldName :: MoneyField -> String
moneyFieldName = \case
  MoneyBegin        -> "obj_f_money_begin"
  MoneyFlags        -> "obj_f_money_flags"
  MoneyQuantity     -> "obj_f_money_quantity"
  MoneyType         -> "obj_f_money_type"
  MoneyPadInt1      -> "obj_f_money_pad_i_1"
  MoneyPadInt2      -> "obj_f_money_pad_i_2"
  MoneyPadInt3      -> "obj_f_money_pad_i_3"
  MoneyPadInt4      -> "obj_f_money_pad_i_4"
  MoneyPadInt5      -> "obj_f_money_pad_i_5"
  MoneyPadIntArr1   -> "obj_f_money_pad_ias_1"
  MoneyPadInt64Arr1 -> "obj_f_money_pad_i64as_1"
  MoneyEnd          -> "obj_f_money_end"

moneyFieldType :: MoneyField -> FieldType
moneyFieldType = \case
  MoneyBegin -> BeginF
  MoneyFlags -> W32F
  MoneyQuantity -> W32F
  MoneyType -> W32F
  MoneyPadInt1 -> W32F
  MoneyPadInt2 -> W32F
  MoneyPadInt3 -> W32F
  MoneyPadInt4 -> W32F
  MoneyPadInt5 -> W32F
  MoneyPadIntArr1 -> W32ArrF
  MoneyPadInt64Arr1 -> W64ArrF
  MoneyEnd -> EndF

parsePartialMoneyFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m MoneyField
parsePartialMoneyFieldName =
  choice
    [ MoneyBegin        <$ chunk "begin"
    , MoneyFlags        <$ chunk "flags"
    , MoneyQuantity     <$ chunk "quantity"
    , MoneyType         <$ chunk "type"
    , MoneyPadInt1      <$ chunk "pad_i_1"
    , MoneyPadInt2      <$ chunk "pad_i_2"
    , MoneyPadInt3      <$ chunk "pad_i_3"
    , MoneyPadInt4      <$ chunk "pad_i_4"
    , MoneyPadInt5      <$ chunk "pad_i_5"
    , MoneyPadIntArr1   <$ chunk "pad_ias_1"
    , MoneyPadInt64Arr1 <$ chunk "pad_i64as_1"
    , MoneyEnd          <$ chunk "end"
    ]

parseMoneyFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m MoneyField
parseMoneyFieldName = chunk "obj_f_money_" *> parsePartialMoneyFieldName

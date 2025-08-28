
module Temple.Object.Field.Bag where

import Temple.Object.Field.Class
import Temple.Object.Field.Type

data BagField
  = BagBegin
  | BagFlags
  | BagSize
  | BagEnd
  deriving (Bounded, Eq, Ord, Show)

instance Enum BagField where
  fromEnum = \case
    BagBegin -> 272
    BagFlags -> 273
    BagSize  -> 274
    BagEnd   -> 275

  toEnum = \case
    272 -> BagBegin
    273 -> BagFlags
    274 -> BagSize
    275 -> BagEnd
    n -> error $ "toEnum @BagField: bad value: " ++ show n

  enumFrom n = enumFromTo n maxBound
  enumFromThen m n = enumFromThenTo m n maxBound

instance Field BagField where
  fieldName = \case
    BagBegin -> "obj_f_bag_begin"
    BagFlags -> "obj_f_bag_flags"
    BagSize  -> "obj_f_bag_size"
    BagEnd   -> "obj_f_bag_end"

  fieldType = \case
    BagBegin -> BeginF
    BagFlags -> W32F
    BagSize -> W32F
    BagEnd -> EndF

  isPadding _ = False

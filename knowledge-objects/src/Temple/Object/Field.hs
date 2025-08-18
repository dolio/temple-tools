
module Temple.Object.Field
  ( module Temple.Object.Field
  , module Temple.Object.Field.General
  , module Temple.Object.Field.Portal
  , module Temple.Object.Field.Container
  , module Temple.Object.Field.Scenery
  , module Temple.Object.Field.Projectile
  , module Temple.Object.Field.Item
  , module Temple.Object.Field.Weapon
  , module Temple.Object.Field.Ammo
  , module Temple.Object.Field.Armor
  , module Temple.Object.Field.Money
  , module Temple.Object.Field.Food
  , module Temple.Object.Field.Scroll
  , module Temple.Object.Field.Key
  , module Temple.Object.Field.Written
  , module Temple.Object.Field.Bag
  , module Temple.Object.Field.Generic
  , module Temple.Object.Field.Critter
  , module Temple.Object.Field.Pc
  , module Temple.Object.Field.Npc
  , module Temple.Object.Field.Trap
  , module Temple.Object.Field.Extra
  ) where

import Temple.Object.Type

import Temple.Object.Field.General
import Temple.Object.Field.Portal
import Temple.Object.Field.Container
import Temple.Object.Field.Scenery
import Temple.Object.Field.Projectile
import Temple.Object.Field.Item
import Temple.Object.Field.Weapon
import Temple.Object.Field.Ammo
import Temple.Object.Field.Armor
import Temple.Object.Field.Money
import Temple.Object.Field.Food
import Temple.Object.Field.Scroll
import Temple.Object.Field.Key
import Temple.Object.Field.Written
import Temple.Object.Field.Bag
import Temple.Object.Field.Generic
import Temple.Object.Field.Critter
import Temple.Object.Field.Pc
import Temple.Object.Field.Npc
import Temple.Object.Field.Trap
import Temple.Object.Field.Extra

import Temple.Object.Field.Type

-- Object fields, split out by the type of objects they belong to.
data ObjectField
  = GeneralF GeneralField
  | PortalF PortalField
  | ContainerF ContainerField
  | SceneryF SceneryField
  | ProjectileF ProjectileField
  | ItemF ItemField
  | WeaponF WeaponField
  | AmmoF AmmoField
  | ArmorF ArmorField
  | MoneyF MoneyField
  | FoodF FoodField
  | ScrollF ScrollField
  | KeyF KeyField
  | WrittenF WrittenField
  | BagF BagField
  | GenericF GenericField
  | CritterF CritterField
  | PcF PcField
  | NpcF NpcField
  | TrapF TrapField
  | ExtraF ExtraField
  deriving (Eq, Ord, Show)

instance Bounded ObjectField where
  minBound = GeneralF minBound
  maxBound = ExtraF maxBound

instance Enum ObjectField where
  fromEnum = \case
    GeneralF gf -> fromEnum gf
    PortalF pf -> fromEnum pf
    ContainerF cf -> fromEnum cf
    SceneryF sf -> fromEnum sf
    ProjectileF pf -> fromEnum pf
    ItemF itf -> fromEnum itf
    WeaponF wf -> fromEnum wf
    AmmoF af -> fromEnum af
    ArmorF af -> fromEnum af
    MoneyF mf -> fromEnum mf
    FoodF ff -> fromEnum ff
    ScrollF sf -> fromEnum sf
    KeyF kf -> fromEnum kf
    WrittenF wf -> fromEnum wf
    BagF bf -> fromEnum bf
    GenericF gf -> fromEnum gf
    CritterF cf -> fromEnum cf
    PcF pf -> fromEnum pf
    NpcF nf -> fromEnum nf
    TrapF tf -> fromEnum tf
    ExtraF ef -> fromEnum ef

  toEnum n
    |   0 <= n, n <=  87 = GeneralF $ toEnum n
    |  88 <= n, n <= 101 = PortalF $ toEnum n
    | 102 <= n, n <= 120 = ContainerF $ toEnum n
    | 121 <= n, n <= 133 = SceneryF $ toEnum n
    | 134 <= n, n <= 150 = ProjectileF $ toEnum n
    | 151 <= n, n <= 186 = ItemF $ toEnum n
    | 187 <= n, n <= 208 = WeaponF $ toEnum n
    | 209 <= n, n <= 218 = AmmoF $ toEnum n
    | 219 <= n, n <= 228 = ArmorF $ toEnum n
    | 229 <= n, n <= 240 = MoneyF $ toEnum n
    | 241 <= n, n <= 247 = FoodF $ toEnum n
    | 248 <= n, n <= 254 = ScrollF $ toEnum n
    | 255 <= n, n <= 261 = KeyF $ toEnum n
    | 262 <= n, n <= 271 = WrittenF $ toEnum n
    | 272 <= n, n <= 275 = BagF $ toEnum n
    | 276 <= n, n <= 282 = GenericF $ toEnum n
    | 283 <= n, n <= 338 = CritterF $ toEnum n
    | 339 <= n, n <= 352 = PcF $ toEnum n
    | 353 <= n, n <= 397 = NpcF $ toEnum n
    | 398 <= n, n <= 404 = TrapF $ toEnum n
    | 405 <= n, n <= 421 = ExtraF $ toEnum n
    | otherwise = error $ "toEnum @ObjectField: bad value: " ++ show n

hasField :: ObjectType -> ObjectField -> Bool
hasField = \cases
  _              (GeneralF    _) -> True
  Portal         (PortalF     _) -> True
  Container      (ContainerF  _) -> True
  Scenery        (SceneryF    _) -> True
  Projectile     (ProjectileF _) -> True
  (Item       _) (ItemF       _) -> True
  (Item  Weapon) (WeaponF     _) -> True
  (Item    Ammo) (AmmoF       _) -> True
  (Item   Armor) (ArmorF      _) -> True
  (Item   Money) (MoneyF      _) -> True
  (Item    Food) (FoodF       _) -> True
  (Item  Scroll) (ScrollF     _) -> True
  (Item     Key) (KeyF        _) -> True
  (Item Written) (WrittenF    _) -> True
  (Item Generic) (GenericF    _) -> True
  (Critter    _) (CritterF    _) -> True
  (Critter   Pc) (PcF         _) -> True
  (Critter  Npc) (NpcF        _) -> True
  (Trap        ) (TrapF       _) -> True
  (Bag         ) (BagF        _) -> True
  _              (ExtraF      _) -> True
  _              _               -> False

fieldName :: ObjectField -> String
fieldName = \case
  GeneralF f -> generalFieldName f
  PortalF f -> portalFieldName f
  ContainerF f -> containerFieldName f
  SceneryF f -> sceneryFieldName f
  ProjectileF f -> projectileFieldName f
  ItemF f -> itemFieldName f
  WeaponF f -> weaponFieldName f
  AmmoF f -> ammoFieldName f
  ArmorF f -> armorFieldName f
  MoneyF f -> moneyFieldName f
  FoodF f -> foodFieldName f
  ScrollF f -> scrollFieldName f
  KeyF f -> keyFieldName f
  WrittenF f -> writtenFieldName f
  BagF f -> bagFieldName f
  GenericF f -> genericFieldName f
  CritterF f -> critterFieldName f
  PcF f -> pcFieldName f
  NpcF f -> npcFieldName f
  TrapF f -> trapFieldName f
  ExtraF f -> extraFieldName f

-- Classifies the types of fields. Currently a `Maybe` since the fields we'll
-- be using/have information for are limited.
fieldType :: ObjectField -> FieldType
fieldType = \case
  GeneralF f -> generalFieldType f
  PortalF f -> portalFieldType f
  ContainerF f -> containerFieldType f
  SceneryF f -> sceneryFieldType f
  ProjectileF f -> projectileFieldType f
  ItemF f -> itemFieldType f
  WeaponF f -> weaponFieldType f
  AmmoF f -> ammoFieldType f
  ArmorF f -> armorFieldType f
  MoneyF f -> moneyFieldType f
  FoodF f -> foodFieldType f
  ScrollF f -> scrollFieldType f
  KeyF f -> keyFieldType f
  WrittenF f -> writtenFieldType f
  BagF f -> bagFieldType f
  GenericF f -> genericFieldType f
  CritterF f -> critterFieldType f
  PcF f -> pcFieldType f
  NpcF f -> npcFieldType f
  TrapF f -> trapFieldType f
  ExtraF f -> extraFieldType f


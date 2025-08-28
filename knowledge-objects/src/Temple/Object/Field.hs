
module Temple.Object.Field
  ( module Temple.Object.Field
  , module Temple.Object.Field.Class
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

import Temple.Object.Field.Class

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
    | 405 <= n, n <= 429 = ExtraF $ toEnum n
    | otherwise = error $ "toEnum @ObjectField: bad value: " ++ show n

  enumFrom n = enumFromTo n maxBound
  enumFromThen m n = enumFromThenTo m n maxBound

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

instance Field ObjectField where
  fieldName = \case
    GeneralF f -> fieldName f
    PortalF f -> fieldName f
    ContainerF f -> fieldName f
    SceneryF f -> fieldName f
    ProjectileF f -> fieldName f
    ItemF f -> fieldName f
    WeaponF f -> fieldName f
    AmmoF f -> fieldName f
    ArmorF f -> fieldName f
    MoneyF f -> fieldName f
    FoodF f -> fieldName f
    ScrollF f -> fieldName f
    KeyF f -> fieldName f
    WrittenF f -> fieldName f
    BagF f -> fieldName f
    GenericF f -> fieldName f
    CritterF f -> fieldName f
    PcF f -> fieldName f
    NpcF f -> fieldName f
    TrapF f -> fieldName f
    ExtraF f -> fieldName f

  fieldType = \case
    GeneralF f -> fieldType f
    PortalF f -> fieldType f
    ContainerF f -> fieldType f
    SceneryF f -> fieldType f
    ProjectileF f -> fieldType f
    ItemF f -> fieldType f
    WeaponF f -> fieldType f
    AmmoF f -> fieldType f
    ArmorF f -> fieldType f
    MoneyF f -> fieldType f
    FoodF f -> fieldType f
    ScrollF f -> fieldType f
    KeyF f -> fieldType f
    WrittenF f -> fieldType f
    BagF f -> fieldType f
    GenericF f -> fieldType f
    CritterF f -> fieldType f
    PcF f -> fieldType f
    NpcF f -> fieldType f
    TrapF f -> fieldType f
    ExtraF f -> fieldType f

  isPadding = \case
    GeneralF f -> isPadding f
    PortalF f -> isPadding f
    ContainerF f -> isPadding f
    SceneryF f -> isPadding f
    ProjectileF f -> isPadding f
    ItemF f -> isPadding f
    WeaponF f -> isPadding f
    AmmoF f -> isPadding f
    ArmorF f -> isPadding f
    MoneyF f -> isPadding f
    FoodF f -> isPadding f
    ScrollF f -> isPadding f
    KeyF f -> isPadding f
    WrittenF f -> isPadding f
    BagF f -> isPadding f
    GenericF f -> isPadding f
    CritterF f -> isPadding f
    PcF f -> isPadding f
    NpcF f -> isPadding f
    TrapF f -> isPadding f
    ExtraF f -> isPadding f

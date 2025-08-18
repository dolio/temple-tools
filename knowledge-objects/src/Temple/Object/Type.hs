
module Temple.Object.Type where

data ItemType
  = Weapon
  | Ammo
  | Armor
  | Money
  | Food
  | Scroll
  | Key
  | Written
  | Generic
  deriving (Bounded, Eq, Ord, Show)

instance Enum ItemType where
  fromEnum = \case
    Weapon  -> 4
    Ammo    -> 5
    Armor   -> 6
    Money   -> 7
    Food    -> 8
    Scroll  -> 9
    Key     -> 10
    Written -> 11
    Generic -> 12

  toEnum = \case
    4  -> Weapon
    5  -> Ammo
    6  -> Armor
    7  -> Money
    8  -> Food
    9  -> Scroll
    10 -> Key
    11 -> Written
    12 -> Generic
    n -> error $ "toEnum @ItemType: bad value: " ++ show n

data CritterType = Pc | Npc
  deriving (Bounded, Eq, Ord, Show)

instance Enum CritterType where
  fromEnum = \case
    Pc -> 13
    Npc -> 14
  toEnum = \case
    13 -> Pc
    14 -> Npc
    n -> error $ "toEnum @Critter: bad value: " ++ show n


-- These are all the object types recognized by ToEE. The Enum instance
-- agrees with the enumeration number used there.
data ObjectType
  = Portal
  | Container
  | Scenery
  | Projectile
  | Item ItemType
  | Critter CritterType
  | Trap
  | Bag
  deriving (Eq, Ord, Show)

instance Bounded ObjectType where
  minBound = Portal
  maxBound = Bag

instance Enum ObjectType where
  fromEnum = \case
    Portal -> 0
    Container -> 1
    Scenery -> 2
    Projectile -> 3
    Item i -> fromEnum i
    Critter c -> fromEnum c
    Trap -> 15
    Bag -> 16

  toEnum = \case
    0 -> Portal
    1 -> Container
    2 -> Scenery
    3 -> Projectile
    4 -> Item Weapon
    5 -> Item Ammo
    6 -> Item Armor
    7 -> Item Money
    8 -> Item Food
    9 -> Item Scroll
    10 -> Item Key
    11 -> Item Written
    12 -> Item Generic
    13 -> Critter Pc
    14 -> Critter Npc
    15 -> Trap
    16 -> Bag
    n -> error $ "toEnum @ObjectType: bad value: " ++ show n

-- ToEE name for object types.
typeName :: ObjectType -> String
typeName = \case
  Portal       -> "obj_t_portal"
  Container    -> "obj_t_container"
  Scenery      -> "obj_t_scenery"
  Projectile   -> "obj_t_projectile"
  Item Weapon  -> "obj_t_weapon"
  Item Ammo    -> "obj_t_ammo"
  Item Armor   -> "obj_t_armor"
  Item Money   -> "obj_t_money"
  Item Food    -> "obj_t_food"
  Item Scroll  -> "obj_t_scroll"
  Item Key     -> "obj_t_key"
  Item Written -> "obj_t_written"
  Item Generic -> "obj_t_generic"
  Critter Pc   -> "obj_t_pc"
  Critter Npc  -> "obj_t_npc"
  Trap         -> "obj_t_trap"
  Bag          -> "obj_t_bag"

-- Read ToEE name
typeFromName :: String -> Maybe ObjectType
typeFromName = \case
  "obj_t_portal"     -> Just Portal
  "obj_t_container"  -> Just Container
  "obj_t_scenery"    -> Just Scenery
  "obj_t_projectile" -> Just Projectile
  "obj_t_weapon"     -> Just $ Item Weapon
  "obj_t_ammo"       -> Just $ Item Ammo
  "obj_t_armor"      -> Just $ Item Armor
  "obj_t_money"      -> Just $ Item Money
  "obj_t_food"       -> Just $ Item Food
  "obj_t_scroll"     -> Just $ Item Scroll
  "obj_t_key"        -> Just $ Item Key
  "obj_t_written"    -> Just $ Item Written
  "obj_t_generic"    -> Just $ Item Generic
  "obj_t_pc"         -> Just $ Critter Pc
  "obj_t_npc"        -> Just $ Critter Npc
  "obj_t_trap"       -> Just Trap
  "obj_t_bag"        -> Just Bag
  _                  -> Nothing


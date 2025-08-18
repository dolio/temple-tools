
module Temple.Objects.Spec where

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

-- These are the categories of scripts an object can respond to.
data ObjectScript
  = SanExamine
  | SanUse
  | SanDestroy
  | SanUnlock
  | SanGet
  | SanDrop
  | SanThrow
  | SanHit
  | SanMiss
  | SanDialog
  | SanFirstHeartbeat
  | SanCatchingThiefPc
  | SanDying
  | SanEnterCombat
  | SanExitCombat
  | SanStartCombat
  | SanEndCombat
  | SanBuyObject
  | SanResurrect
  | SanHeartbeat
  | SanLeaderKilling
  | SanInsertItem
  | SanWillKos
  | SanTakingDamage
  | SanWieldOn
  | SanWieldOff
  | SanCritterHits
  | SanNewSector
  | SanRemoveItem
  | SanLeaderSleeping
  | SanBust
  | SanDialogOverride
  | SanTransfer
  | SanCaughtThief
  | SanCriticalHit
  | SanCriticalMiss
  | SanJoin
  | SanDisband
  | SanNewMap
  | SanTrap
  | SanTrueSeeing
  | SanSpellCast
  | SanUnlockAttempt
  deriving (Bounded, Enum, Eq, Ord, Show)

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

-- The type of data that occurs in a field
--
-- Note: Temple+ says location fields are Int64, because the location is
-- normally packed. But it seems to be stored differently in e.g. MOB files,
-- so it's worth distinguishing.
data FieldType
  = BeginF      -- beginning of section
  | EndF        -- ending of section
  | NoneF       -- probably shouldn't be used for anything, like the above
  | W32F        -- unsigned 32-bit integer
  | W64F        -- unsigned 64-bit integer
  | I32F        -- signed 32-bit integer
  | F32F        -- 32-bit float
  | B32F        -- 32-bit boolean representation
  | LocF        -- location, 2 32-bit integers
  | ObjF        -- object field (probably UUID in serialized cases)
  | StringF     -- string
  | W32ArrF     -- array of unsigned 32-bit integers
  | W64ArrF     -- array of unsigned 64-bit integers
  | ObjArrF     -- array of objects
  | StandptArrF -- array of standpoints
  | WayptArrF   -- array of waypoints
  | AbilityArrF -- array of ability scores
  | ScriptArrF  -- array of scripts (numbers, probably)
  | SpellArrF   -- array of spell entries
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

-- --------------------
-- Specific field types
-- --------------------

-- General fields belong to every object
data GeneralField
  = GeneralBegin
  | Location
  | XOffset
  | YOffset
  | ShadowArt2D
  | BlitFlags
  | BlitColor
  | Transparency
  | ModelScale
  | LightFlags
  | LightMaterial
  | LightColor
  | LightRadius
  | LightAngleStart
  | LightAngleEnd
  | LightType
  | LightXFacing
  | LightYFacing
  | LightZFacing
  | LightXOffset
  | LightYOffset
  | LightZOffset
  | Flags
  | SpellFlags
  | Name
  | Description
  | Size
  -- Above here, the names may not be accurate
  | HpPts
  | HpAdj
  | HpDamage
  | Material
  | ScriptsIdx
  | SoundEffect
  | Category
  | Rotation
  | SpeedWalk
  | SpeedRun
  | BaseMesh
  | BaseAnim
  | Radius
  | RenderHeight3D
  | Conditions
  | ConditionArg0
  | PermanentMods
  | Initiative
  | Dispatcher
  | Subinitiative
  | SecretdoorFlags
  | SecretdoorEffectname
  | SecretdoorDC
  | PadInt7
  | PadInt8
  | PadInt9
  | PadInt0
  | ZOffset
  | RotationPitch
  | PadFloat3
  | PadFloat4
  | PadFloat5
  | PadFloat6
  | PadFloat7
  | PadFloat8
  | PadFloat9
  | PadFloat0
  | PadInt640
  | PadInt641
  | PadInt642
  | PadInt643
  | PadInt644
  | LastHitBy
  | PadObj1
  | PadObj2
  | PadObj3
  | PadObj4
  | PermanentModData
  | AttackTypesIdx
  | AttackBonusIdx
  | StrategyState
  | PadIntArr4
  | PadInt64Arr0
  | PadInt64Arr1
  | PadInt64Arr2
  | PadInt64Arr3
  | PadInt64Arr4
  | PadObjArr0
  | PadObjArr1
  | PadObjArr2
  | GeneralEnd
  deriving (Bounded, Enum, Eq, Ord, Show)

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

data ProjectileField
  = ProjectileBegin
  | ProjectileFlagsCombat
  | ProjectileFlagsCombatDamage
  | ProjectileParentWeapon
  | ProjectileParentAmmo
  | ProjectilePartSysId
  | ProjectileAccelerationX
  | ProjectileAccelerationY
  | ProjectileAccelerationZ
  | ProjectilePadInt4
  | ProjectilePadObj1
  | ProjectilePadObj2
  | ProjectilePadObj3
  | ProjectilePadIntArr1
  | ProjectilePadInt64Arr1
  | ProjectilePadObjArr1
  | ProjectileEnd
  deriving (Bounded, Eq, Ord, Show)

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

data WeaponField
  = WeaponBegin
  | WeaponFlags
  | WeaponRange
  | WeaponAmmoType
  | WeaponAmmoConsumption
  | WeaponMissileAid
  | WeaponCritHitChart
  | WeaponAttacktype
  | WeaponDamageDice
  | WeaponAnimtype
  | WeaponType
  | WeaponCritRange
  | WeaponPadInt1
  | WeaponPadInt2
  | WeaponPadObj1
  | WeaponPadObj2
  | WeaponPadObj3
  | WeaponPadObj4
  | WeaponPadObj5
  | WeaponPadIntArr1
  | WeaponPadInt64Arr1
  | WeaponEnd
  deriving (Bounded, Eq, Ord, Show)

data AmmoField
  = AmmoBegin
  | AmmoFlags
  | AmmoQuantity
  | AmmoType
  | AmmoPadInt1
  | AmmoPadInt2
  | AmmoPadObj1
  | AmmoPadIntArr1
  | AmmoPadInt64Arr1
  | AmmoEnd
  deriving (Bounded, Eq, Ord, Show)

data ArmorField
  = ArmorBegin
  | ArmorFlags
  | ArmorAcAdj
  | ArmorMaxDexBonus
  | ArmorArcaneSpellFailure
  | ArmorArmorCheckPenalty
  | ArmorPadInt1
  | ArmorPadIntArr1
  | ArmorPadInt64Arr1
  | ArmorEnd
  deriving (Bounded, Eq, Ord, Show)

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

data FoodField
  = FoodBegin
  | FoodFlags
  | FoodPadInt1
  | FoodPadInt2
  | FoodPadIntArr1
  | FoodPadInt64Arr1
  | FoodEnd
  deriving (Bounded, Eq, Ord, Show)

data ScrollField
  = ScrollBegin
  | ScrollFlags
  | ScrollPadInt1
  | ScrollPadInt2
  | ScrollPadIntArr1
  | ScrollPadInt64Arr1
  | ScrollEnd
  deriving (Bounded, Eq, Ord, Show)

data KeyField
  = KeyBegin
  | KeyKeyId
  | KeyPadInt1
  | KeyPadInt2
  | KeyPadIntArr1
  | KeyPadInt64Arr1
  | KeyEnd
  deriving (Bounded, Eq, Ord, Show)

data WrittenField
  = WrittenBegin
  | WrittenFlags
  | WrittenSubtype
  | WrittenTextStartLine
  | WrittenTextEndLine
  | WrittenPadInt1
  | WrittenPadInt2
  | WrittenPadIntArr1
  | WrittenPadInt64Arr1
  | WrittenEnd
  deriving (Bounded, Eq, Ord, Show)

data BagField
  = BagBegin
  | BagFlags
  | BagSize
  | BagEnd
  deriving (Bounded, Eq, Ord, Show)

-- 'Generic' is (I think) a category for items that don't fit into one of
-- the above categories. A common use is to spawn one as the anchor for an
-- AoE spell.
data GenericField
  = GenericBegin
  | GenericFlags
  | GenericUsageBonus
  | GenericUsageCountRemaining
  | GenericPadIntArr1
  | GenericPadInt64Arr1
  | GenericEnd
  deriving (Bounded, Eq, Ord, Show)

-- Critters encompass both PCs and NPCs/monsters.
data CritterField
  = CritterBegin
  | CritterFlags
  | CritterFlags2
  | CritterAbilitiesIdx
  | CritterLevelIdx
  | CritterRace
  | CritterGender
  | CritterAge
  | CritterHeight
  | CritterWeight
  | CritterExperience
  | CritterPadInt1
  | CritterAlignment
  | CritterDeity
  | CritterDomain1
  | CritterDomain2
  | CritterAlignmentChoice
  | CritterSchoolSpecialization
  | CritterSpellsKnownIdx
  | CritterSpellsMemorizedIdx
  | CritterSpellsCastIdx
  | CritterFeatIdx
  | CritterFeatCountIdx
  | CritterFleeingFrom
  | CritterPortrait
  | CritterMoneyIdx
  | CritterInventoryNum
  | CritterInventoryListIdx
  | CritterInventorySource
  | CritterDescriptionUnknown
  | CritterFollowerIdx
  | CritterTeleportDest
  | CritterTeleportMap
  | CritterDeathTime
  | CritterSkillIdx
  | CritterReach
  | CritterSubdualDamage
  | CritterPadInt4 -- level up scheme?
  | CritterPadInt5
  | CritterSequence
  | CritterHairStyle
  | CritterStrategy
  | CritterPadInt3
  | CritterMonsterCategory
  | CritterPadInt642
  | CritterPadInt643
  | CritterPadInt644
  | CritterPadInt645
  | CritterDamageIdx
  | CritterAttacksIdx
  | CritterSeenMaplist
  | CritterPadInt64Arr2
  | CritterPadInt64Arr3
  | CritterPadInt64Arr4
  | CritterPadInt64Arr5
  | CritterEnd
  deriving (Bounded, Eq, Ord, Show)

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

data NpcField
  = NpcBegin
  | NpcFlags
  | NpcLeader
  | NpcAiData
  | NpcCombatFocus
  | NpcWhoHitMeLast
  | NpcWaypointsIdx
  | NpcWaypointCurrent
  | NpcStandpointDayINVALID
  | NpcStandpointNightINVALID
  | NpcFaction
  | NpcRetailPriceMultiplier
  | NpcSubstituteInventory
  | NpcReactionBase
  | NpcChallengeRating
  | NpcReactionPcIdx
  | NpcReactionLevelIdx
  | NpcReactionTimeIdx
  | NpcGeneratorData
  | NpcAiListIdx
  | NpcSaveReflexesBonus
  | NpcSaveFortitudeBonus
  | NpcSaveWillpowerBonus
  | NpcAcBonus
  | NpcAddMesh
  | NpcWaypointAnim
  | NpcPadInt3
  | NpcPadInt4
  | NpcPadInt5
  | NpcAiFlags64
  | NpcPadInt642
  | NpcPadInt643
  | NpcPadInt644
  | NpcPadInt645
  | NpcHitdiceIdx
  | NpcAiListTypeIdx
  | NpcPadIntArr3
  | NpcPadIntArr4
  | NpcPadIntArr5
  | NpcStandpoints
  | NpcPadInt64Arr2
  | NpcPadInt64Arr3
  | NpcPadInt64Arr4
  | NpcPadInt64Arr5
  | NpcEnd
  deriving (Bounded, Eq, Ord, Show)

data TrapField
  = TrapBegin
  | TrapFlags
  | TrapDifficulty
  | TrapPadInt2
  | TrapPadIntArr1
  | TrapPadInt64Arr1
  | TrapEnd
  deriving (Bounded, Eq, Ord, Show)

-- These seem to be general flags that were probably added later.
data ExtraField
  = TotalNormal
  | TransientBegin
  | RenderColor
  | RenderColors
  | RenderPalette
  | RenderScale
  | RenderAlpha
  | RenderX
  | RenderY
  | RenderWidth
  | RenderHeight
  | Palette
  | Color
  | Colors
  | RenderFlags
  | TempId
  | LightHandle
  | OverlayLightHandles
  | InternalFlags
  | FindNode
  | AnimationHandle
  | GrappleState
  | TransientEnd
  | Type
  | PrototypeHandle
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

instance Enum ProjectileField where
  fromEnum = \case
    ProjectileBegin             -> 134
    ProjectileFlagsCombat       -> 135
    ProjectileFlagsCombatDamage -> 136
    ProjectileParentWeapon      -> 137
    ProjectileParentAmmo        -> 138
    ProjectilePartSysId         -> 139
    ProjectileAccelerationX     -> 140
    ProjectileAccelerationY     -> 141
    ProjectileAccelerationZ     -> 142
    ProjectilePadInt4           -> 143
    ProjectilePadObj1           -> 144
    ProjectilePadObj2           -> 145
    ProjectilePadObj3           -> 146
    ProjectilePadIntArr1        -> 147
    ProjectilePadInt64Arr1      -> 148
    ProjectilePadObjArr1        -> 149
    ProjectileEnd               -> 150

  toEnum = \case
    134 -> ProjectileBegin
    135 -> ProjectileFlagsCombat
    136 -> ProjectileFlagsCombatDamage
    137 -> ProjectileParentWeapon
    138 -> ProjectileParentAmmo
    139 -> ProjectilePartSysId
    140 -> ProjectileAccelerationX
    141 -> ProjectileAccelerationY
    142 -> ProjectileAccelerationZ
    143 -> ProjectilePadInt4
    144 -> ProjectilePadObj1
    145 -> ProjectilePadObj2
    146 -> ProjectilePadObj3
    147 -> ProjectilePadIntArr1
    148 -> ProjectilePadInt64Arr1
    149 -> ProjectilePadObjArr1
    150 -> ProjectileEnd
    n -> error $ "toEnum @ProjectileField: bad value: " ++ show n

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

instance Enum WeaponField where
  fromEnum = \case
    WeaponBegin           -> 187
    WeaponFlags           -> 188
    WeaponRange           -> 189
    WeaponAmmoType        -> 190
    WeaponAmmoConsumption -> 191
    WeaponMissileAid      -> 192
    WeaponCritHitChart    -> 193
    WeaponAttacktype      -> 194
    WeaponDamageDice      -> 195
    WeaponAnimtype        -> 196
    WeaponType            -> 197
    WeaponCritRange       -> 198
    WeaponPadInt1         -> 199
    WeaponPadInt2         -> 200
    WeaponPadObj1         -> 201
    WeaponPadObj2         -> 202
    WeaponPadObj3         -> 203
    WeaponPadObj4         -> 204
    WeaponPadObj5         -> 205
    WeaponPadIntArr1      -> 206
    WeaponPadInt64Arr1    -> 207
    WeaponEnd             -> 208

  toEnum = \case
    187 -> WeaponBegin
    188 -> WeaponFlags
    189 -> WeaponRange
    190 -> WeaponAmmoType
    191 -> WeaponAmmoConsumption
    192 -> WeaponMissileAid
    193 -> WeaponCritHitChart
    194 -> WeaponAttacktype
    195 -> WeaponDamageDice
    196 -> WeaponAnimtype
    197 -> WeaponType
    198 -> WeaponCritRange
    199 -> WeaponPadInt1
    200 -> WeaponPadInt2
    201 -> WeaponPadObj1
    202 -> WeaponPadObj2
    203 -> WeaponPadObj3
    204 -> WeaponPadObj4
    205 -> WeaponPadObj5
    206 -> WeaponPadIntArr1
    207 -> WeaponPadInt64Arr1
    208 -> WeaponEnd
    n -> error $ "toEnum @WeaponField: bad value: " ++ show n

instance Enum AmmoField where
  fromEnum = \case
    AmmoBegin        -> 209
    AmmoFlags        -> 210
    AmmoQuantity     -> 211
    AmmoType         -> 212
    AmmoPadInt1      -> 213
    AmmoPadInt2      -> 214
    AmmoPadObj1      -> 215
    AmmoPadIntArr1   -> 216
    AmmoPadInt64Arr1 -> 217
    AmmoEnd          -> 218

  toEnum = \case
    209 -> AmmoBegin
    210 -> AmmoFlags
    211 -> AmmoQuantity
    212 -> AmmoType
    213 -> AmmoPadInt1
    214 -> AmmoPadInt2
    215 -> AmmoPadObj1
    216 -> AmmoPadIntArr1
    217 -> AmmoPadInt64Arr1
    218 -> AmmoEnd
    n -> error $ "toEnum @AmmoField: bad value: " ++ show n

instance Enum ArmorField where
  fromEnum = \case
    ArmorBegin              -> 219
    ArmorFlags              -> 220
    ArmorAcAdj              -> 221
    ArmorMaxDexBonus        -> 222
    ArmorArcaneSpellFailure -> 223
    ArmorArmorCheckPenalty  -> 224
    ArmorPadInt1            -> 225
    ArmorPadIntArr1         -> 226
    ArmorPadInt64Arr1       -> 227
    ArmorEnd                -> 228

  toEnum = \case
    219 -> ArmorBegin
    220 -> ArmorFlags
    221 -> ArmorAcAdj
    222 -> ArmorMaxDexBonus
    223 -> ArmorArcaneSpellFailure
    224 -> ArmorArmorCheckPenalty
    225 -> ArmorPadInt1
    226 -> ArmorPadIntArr1
    227 -> ArmorPadInt64Arr1
    228 -> ArmorEnd
    n -> error $ "toEnum @ArmorField: bad value: " ++ show n

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

instance Enum FoodField where
  fromEnum = \case
    FoodBegin        -> 241
    FoodFlags        -> 242
    FoodPadInt1      -> 243
    FoodPadInt2      -> 244
    FoodPadIntArr1   -> 245
    FoodPadInt64Arr1 -> 246
    FoodEnd          -> 247

  toEnum = \case
    241 -> FoodBegin
    242 -> FoodFlags
    243 -> FoodPadInt1
    244 -> FoodPadInt2
    245 -> FoodPadIntArr1
    246 -> FoodPadInt64Arr1
    247 -> FoodEnd
    n -> error $ "toEnum @FoodField: bad value: " ++ show n

instance Enum ScrollField where
  fromEnum = \case
    ScrollBegin        -> 248
    ScrollFlags        -> 249
    ScrollPadInt1      -> 250
    ScrollPadInt2      -> 251
    ScrollPadIntArr1   -> 252
    ScrollPadInt64Arr1 -> 253
    ScrollEnd          -> 254
  toEnum = \case
    248 -> ScrollBegin
    249 -> ScrollFlags
    250 -> ScrollPadInt1
    251 -> ScrollPadInt2
    252 -> ScrollPadIntArr1
    253 -> ScrollPadInt64Arr1
    254 -> ScrollEnd
    n -> error $ "toEnum @ScrollField: bad value: " ++ show n

instance Enum KeyField where
  fromEnum = \case
    KeyBegin        -> 255
    KeyKeyId        -> 256
    KeyPadInt1      -> 257
    KeyPadInt2      -> 258
    KeyPadIntArr1   -> 259
    KeyPadInt64Arr1 -> 260
    KeyEnd          -> 261

  toEnum = \case
    255 -> KeyBegin
    256 -> KeyKeyId
    257 -> KeyPadInt1
    258 -> KeyPadInt2
    259 -> KeyPadIntArr1
    260 -> KeyPadInt64Arr1
    261 -> KeyEnd
    n -> error $ "toEnum @KeyField: bad value: " ++ show n

instance Enum WrittenField where
  fromEnum = \case
    WrittenBegin         -> 262
    WrittenFlags         -> 263
    WrittenSubtype       -> 264
    WrittenTextStartLine -> 265
    WrittenTextEndLine   -> 266
    WrittenPadInt1       -> 267
    WrittenPadInt2       -> 268
    WrittenPadIntArr1    -> 269
    WrittenPadInt64Arr1  -> 270
    WrittenEnd           -> 271

  toEnum = \case
    262 -> WrittenBegin
    263 -> WrittenFlags
    264 -> WrittenSubtype
    265 -> WrittenTextStartLine
    266 -> WrittenTextEndLine
    267 -> WrittenPadInt1
    268 -> WrittenPadInt2
    269 -> WrittenPadIntArr1
    270 -> WrittenPadInt64Arr1
    271 -> WrittenEnd
    n -> error $ "toEnum @WrittenField: bad value: " ++ show n

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

instance Enum GenericField where
  fromEnum = \case
    GenericBegin               -> 276
    GenericFlags               -> 277
    GenericUsageBonus          -> 278
    GenericUsageCountRemaining -> 279
    GenericPadIntArr1          -> 280
    GenericPadInt64Arr1        -> 281
    GenericEnd                 -> 282

  toEnum = \case
    276 -> GenericBegin
    277 -> GenericFlags
    278 -> GenericUsageBonus
    279 -> GenericUsageCountRemaining
    280 -> GenericPadIntArr1
    281 -> GenericPadInt64Arr1
    282 -> GenericEnd
    n -> error $ "toEnum @GenericField: bad value: " ++ show n

instance Enum CritterField where
  fromEnum = \case
    CritterBegin                -> 283
    CritterFlags                -> 284
    CritterFlags2               -> 285
    CritterAbilitiesIdx         -> 286
    CritterLevelIdx             -> 287
    CritterRace                 -> 288
    CritterGender               -> 289
    CritterAge                  -> 290
    CritterHeight               -> 291
    CritterWeight               -> 292
    CritterExperience           -> 293
    CritterPadInt1              -> 294
    CritterAlignment            -> 295
    CritterDeity                -> 296
    CritterDomain1              -> 297
    CritterDomain2              -> 298
    CritterAlignmentChoice      -> 299
    CritterSchoolSpecialization -> 300
    CritterSpellsKnownIdx       -> 301
    CritterSpellsMemorizedIdx   -> 302
    CritterSpellsCastIdx        -> 303
    CritterFeatIdx              -> 304
    CritterFeatCountIdx         -> 305
    CritterFleeingFrom          -> 306
    CritterPortrait             -> 307
    CritterMoneyIdx             -> 308
    CritterInventoryNum         -> 309
    CritterInventoryListIdx     -> 310
    CritterInventorySource      -> 311
    CritterDescriptionUnknown   -> 312
    CritterFollowerIdx          -> 313
    CritterTeleportDest         -> 314
    CritterTeleportMap          -> 315
    CritterDeathTime            -> 316
    CritterSkillIdx             -> 317
    CritterReach                -> 318
    CritterSubdualDamage        -> 319
    CritterPadInt4              -> 320
    CritterPadInt5              -> 321
    CritterSequence             -> 322
    CritterHairStyle            -> 323
    CritterStrategy             -> 324
    CritterPadInt3              -> 325
    CritterMonsterCategory      -> 326
    CritterPadInt642            -> 327
    CritterPadInt643            -> 328
    CritterPadInt644            -> 329
    CritterPadInt645            -> 330
    CritterDamageIdx            -> 331
    CritterAttacksIdx           -> 332
    CritterSeenMaplist          -> 333
    CritterPadInt64Arr2         -> 334
    CritterPadInt64Arr3         -> 335
    CritterPadInt64Arr4         -> 336
    CritterPadInt64Arr5         -> 337
    CritterEnd                  -> 338

  toEnum = \case
    283 -> CritterBegin
    284 -> CritterFlags
    285 -> CritterFlags2
    286 -> CritterAbilitiesIdx
    287 -> CritterLevelIdx
    288 -> CritterRace
    289 -> CritterGender
    290 -> CritterAge
    291 -> CritterHeight
    292 -> CritterWeight
    293 -> CritterExperience
    294 -> CritterPadInt1
    295 -> CritterAlignment
    296 -> CritterDeity
    297 -> CritterDomain1
    298 -> CritterDomain2
    299 -> CritterAlignmentChoice
    300 -> CritterSchoolSpecialization
    301 -> CritterSpellsKnownIdx
    302 -> CritterSpellsMemorizedIdx
    303 -> CritterSpellsCastIdx
    304 -> CritterFeatIdx
    305 -> CritterFeatCountIdx
    306 -> CritterFleeingFrom
    307 -> CritterPortrait
    308 -> CritterMoneyIdx
    309 -> CritterInventoryNum
    310 -> CritterInventoryListIdx
    311 -> CritterInventorySource
    312 -> CritterDescriptionUnknown
    313 -> CritterFollowerIdx
    314 -> CritterTeleportDest
    315 -> CritterTeleportMap
    316 -> CritterDeathTime
    317 -> CritterSkillIdx
    318 -> CritterReach
    319 -> CritterSubdualDamage
    320 -> CritterPadInt4
    321 -> CritterPadInt5
    322 -> CritterSequence
    323 -> CritterHairStyle
    324 -> CritterStrategy
    325 -> CritterPadInt3
    326 -> CritterMonsterCategory
    327 -> CritterPadInt642
    328 -> CritterPadInt643
    329 -> CritterPadInt644
    330 -> CritterPadInt645
    331 -> CritterDamageIdx
    332 -> CritterAttacksIdx
    333 -> CritterSeenMaplist
    334 -> CritterPadInt64Arr2
    335 -> CritterPadInt64Arr3
    336 -> CritterPadInt64Arr4
    337 -> CritterPadInt64Arr5
    338 -> CritterEnd
    n -> error $ "toEnum @CritterField: bad value: " ++ show n

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

instance Enum NpcField where
  fromEnum = \case
    NpcBegin                           -> 353
    NpcFlags                           -> 354
    NpcLeader                          -> 355
    NpcAiData                          -> 356
    NpcCombatFocus                     -> 357
    NpcWhoHitMeLast                    -> 358
    NpcWaypointsIdx                    -> 359
    NpcWaypointCurrent                 -> 360
    NpcStandpointDayINVALID            -> 361
    NpcStandpointNightINVALID          -> 362
    NpcFaction                         -> 363
    NpcRetailPriceMultiplier           -> 364
    NpcSubstituteInventory             -> 365
    NpcReactionBase                    -> 366
    NpcChallengeRating                 -> 367
    NpcReactionPcIdx                   -> 368
    NpcReactionLevelIdx                -> 369
    NpcReactionTimeIdx                 -> 370
    NpcGeneratorData                   -> 371
    NpcAiListIdx                       -> 372
    NpcSaveReflexesBonus               -> 373
    NpcSaveFortitudeBonus              -> 374
    NpcSaveWillpowerBonus              -> 375
    NpcAcBonus                         -> 376
    NpcAddMesh                         -> 377
    NpcWaypointAnim                    -> 378
    NpcPadInt3                         -> 379
    NpcPadInt4                         -> 380
    NpcPadInt5                         -> 381
    NpcAiFlags64                       -> 382
    NpcPadInt642                       -> 383
    NpcPadInt643                       -> 384
    NpcPadInt644                       -> 385
    NpcPadInt645                       -> 386
    NpcHitdiceIdx                      -> 387
    NpcAiListTypeIdx                   -> 388
    NpcPadIntArr3                      -> 389
    NpcPadIntArr4                      -> 390
    NpcPadIntArr5                      -> 391
    NpcStandpoints                     -> 392
    NpcPadInt64Arr2                    -> 393
    NpcPadInt64Arr3                    -> 394
    NpcPadInt64Arr4                    -> 395
    NpcPadInt64Arr5                    -> 396
    NpcEnd                             -> 397

  toEnum = \case
    353 -> NpcBegin
    354 -> NpcFlags
    355 -> NpcLeader
    356 -> NpcAiData
    357 -> NpcCombatFocus
    358 -> NpcWhoHitMeLast
    359 -> NpcWaypointsIdx
    360 -> NpcWaypointCurrent
    361 -> NpcStandpointDayINVALID
    362 -> NpcStandpointNightINVALID
    363 -> NpcFaction
    364 -> NpcRetailPriceMultiplier
    365 -> NpcSubstituteInventory
    366 -> NpcReactionBase
    367 -> NpcChallengeRating
    368 -> NpcReactionPcIdx
    369 -> NpcReactionLevelIdx
    370 -> NpcReactionTimeIdx
    371 -> NpcGeneratorData
    372 -> NpcAiListIdx
    373 -> NpcSaveReflexesBonus
    374 -> NpcSaveFortitudeBonus
    375 -> NpcSaveWillpowerBonus
    376 -> NpcAcBonus
    377 -> NpcAddMesh
    378 -> NpcWaypointAnim
    379 -> NpcPadInt3
    380 -> NpcPadInt4
    381 -> NpcPadInt5
    382 -> NpcAiFlags64
    383 -> NpcPadInt642
    384 -> NpcPadInt643
    385 -> NpcPadInt644
    386 -> NpcPadInt645
    387 -> NpcHitdiceIdx
    388 -> NpcAiListTypeIdx
    389 -> NpcPadIntArr3
    390 -> NpcPadIntArr4
    391 -> NpcPadIntArr5
    392 -> NpcStandpoints
    393 -> NpcPadInt64Arr2
    394 -> NpcPadInt64Arr3
    395 -> NpcPadInt64Arr4
    396 -> NpcPadInt64Arr5
    397 -> NpcEnd
    n -> error $ "toEnum @NpcField: bad value: " ++ show n

instance Enum TrapField where
  fromEnum = \case
    TrapBegin        -> 398
    TrapFlags        -> 399
    TrapDifficulty   -> 400
    TrapPadInt2      -> 401
    TrapPadIntArr1   -> 402
    TrapPadInt64Arr1 -> 403
    TrapEnd          -> 404

  toEnum = \case
    398 -> TrapBegin
    399 -> TrapFlags
    400 -> TrapDifficulty
    401 -> TrapPadInt2
    402 -> TrapPadIntArr1
    403 -> TrapPadInt64Arr1
    404 -> TrapEnd
    n -> error $ "toEnum @TrapField: bad value: " ++ show n

instance Enum ExtraField where
  fromEnum = \case
    TotalNormal         -> 405
    TransientBegin      -> 406
    RenderColor         -> 407
    RenderColors        -> 408
    RenderPalette       -> 409
    RenderScale         -> 410
    RenderAlpha         -> 411
    RenderX             -> 412
    RenderY             -> 413
    RenderWidth         -> 414
    RenderHeight        -> 415
    Palette             -> 416
    Color               -> 417
    Colors              -> 418
    RenderFlags         -> 419
    TempId              -> 420
    LightHandle         -> 421
    OverlayLightHandles -> 422
    InternalFlags       -> 423
    FindNode            -> 424
    AnimationHandle     -> 425
    GrappleState        -> 426
    TransientEnd        -> 427
    Type                -> 428
    PrototypeHandle     -> 429

  toEnum = \case
    405 -> TotalNormal
    406 -> TransientBegin
    407 -> RenderColor
    408 -> RenderColors
    409 -> RenderPalette
    410 -> RenderScale
    411 -> RenderAlpha
    412 -> RenderX
    413 -> RenderY
    414 -> RenderWidth
    415 -> RenderHeight
    416 -> Palette
    417 -> Color
    418 -> Colors
    419 -> RenderFlags
    420 -> TempId
    421 -> LightHandle
    422 -> OverlayLightHandles
    423 -> InternalFlags
    424 -> FindNode
    425 -> AnimationHandle
    426 -> GrappleState
    427 -> TransientEnd
    428 -> Type
    429 -> PrototypeHandle
    n -> error $ "toEnum @ExtraField: bad value: " ++ show n

generalFieldName :: GeneralField -> String
generalFieldName = \case
  GeneralBegin -> "obj_f_begin"
  Location -> "obj_f_location"
  XOffset -> "obj_f_offset_x"
  YOffset -> "obj_f_offset_y"
  ShadowArt2D -> "obj_f_2D_shadow_art"
  BlitFlags -> "obj_f_blit_flags"
  BlitColor -> "obj_f_blit_color"
  Transparency -> "obj_f_transparency"
  ModelScale -> "obj_f_model_scale"
  LightFlags -> "obj_f_light_flags"
  LightMaterial -> "obj_f_light_material"
  LightColor -> "obj_f_light_color"
  LightRadius -> "obj_f_light_radius"
  LightAngleStart -> "obj_f_light_angle_start"
  LightAngleEnd -> "obj_f_light_angle_end"
  LightType -> "obj_f_light_type"
  LightXFacing -> "obj_f_light_facing_X"
  LightYFacing -> "obj_f_light_facing_Y"
  LightZFacing -> "obj_f_light_facing_Z"
  LightXOffset -> "obj_f_light_offset_X"
  LightYOffset -> "obj_f_light_offset_Y"
  LightZOffset -> "obj_f_light_offset_Z"
  Flags -> "obj_f_flags"
  SpellFlags -> "obj_f_spell_flags"
  Name -> "obj_f_name"
  Description -> "obj_f_description"
  Size -> "obj_f_size"
  HpPts -> "obj_f_hp_pts"
  HpAdj -> "obj_f_hp_adj"
  HpDamage -> "obj_f_hp_damage"
  Material -> "obj_f_material"
  ScriptsIdx -> "obj_f_scripts_idx"
  SoundEffect -> "obj_f_sound_effect"
  Category -> "obj_f_category"
  Rotation -> "obj_f_rotation"
  SpeedWalk -> "obj_f_speed_walk"
  SpeedRun -> "obj_f_speed_run"
  BaseMesh -> "obj_f_base_mesh"
  BaseAnim -> "obj_f_base_anim"
  Radius -> "obj_f_radius"
  RenderHeight3D -> "obj_f_3d_render_height"
  Conditions -> "obj_f_conditions"
  ConditionArg0 -> "obj_f_condition_arg0"
  PermanentMods -> "obj_f_permanent_mods"
  Initiative -> "obj_f_initiative"
  Dispatcher -> "obj_f_dispatcher"
  Subinitiative -> "obj_f_subinitiative"
  SecretdoorFlags -> "obj_f_secretdoor_flags"
  SecretdoorEffectname -> "obj_f_secretdoor_effectname"
  SecretdoorDC -> "obj_f_secretdoor_dc"
  PadInt7 -> "obj_f_pad_i_7"
  PadInt8 -> "obj_f_pad_i_8"
  PadInt9 -> "obj_f_pad_i_9"
  PadInt0 -> "obj_f_pad_i_0"
  ZOffset -> "obj_f_offset_z"
  RotationPitch -> "obj_f_rotation_pitch"
  PadFloat3 -> "obj_f_pad_f_3"
  PadFloat4 -> "obj_f_pad_f_4"
  PadFloat5 -> "obj_f_pad_f_5"
  PadFloat6 -> "obj_f_pad_f_6"
  PadFloat7 -> "obj_f_pad_f_7"
  PadFloat8 -> "obj_f_pad_f_8"
  PadFloat9 -> "obj_f_pad_f_9"
  PadFloat0 -> "obj_f_pad_f_0"
  PadInt640 -> "obj_f_pad_i64_0"
  PadInt641 -> "obj_f_pad_i64_1"
  PadInt642 -> "obj_f_pad_i64_2"
  PadInt643 -> "obj_f_pad_i64_3"
  PadInt644 -> "obj_f_pad_i64_4"
  LastHitBy -> "obj_f_last_hit_by"
  PadObj1 -> "obj_f_pad_obj_1"
  PadObj2 -> "obj_f_pad_obj_2"
  PadObj3 -> "obj_f_pad_obj_3"
  PadObj4 -> "obj_f_pad_obj_4"
  PermanentModData -> "obj_f_permanent_mod_data"
  AttackTypesIdx -> "obj_f_attack_types_idx"
  AttackBonusIdx -> "obj_f_attack_bonus_idx"
  StrategyState -> "obj_f_strategy_state"
  PadIntArr4 -> "obj_f_pad_ias_4"
  PadInt64Arr0 -> "obj_f_pad_i64as_0"
  PadInt64Arr1 -> "obj_f_pad_i64as_1"
  PadInt64Arr2 -> "obj_f_pad_i64as_2"
  PadInt64Arr3 -> "obj_f_pad_i64as_3"
  PadInt64Arr4 -> "obj_f_pad_i64as_4"
  PadObjArr0 -> "obj_f_pad_objas_0"
  PadObjArr1 -> "obj_f_pad_objas_1"
  PadObjArr2 -> "obj_f_pad_objas_2"
  GeneralEnd -> "obj_f_end"

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

projectileFieldName :: ProjectileField -> String
projectileFieldName = \case
  ProjectileBegin             -> "obj_f_projectile_begin"
  ProjectileFlagsCombat       -> "obj_f_projectile_flags_combat"
  ProjectileFlagsCombatDamage -> "obj_f_projectile_flags_combat_damage"
  ProjectileParentWeapon      -> "obj_f_projectile_parent_weapon"
  ProjectileParentAmmo        -> "obj_f_projectile_parent_ammo"
  ProjectilePartSysId         -> "obj_f_projectile_part_sys_id"
  ProjectileAccelerationX     -> "obj_f_projectile_acceleration_x"
  ProjectileAccelerationY     -> "obj_f_projectile_acceleration_y"
  ProjectileAccelerationZ     -> "obj_f_projectile_acceleration_z"
  ProjectilePadInt4           -> "obj_f_projectile_pad_i_4"
  ProjectilePadObj1           -> "obj_f_projectile_pad_obj_1"
  ProjectilePadObj2           -> "obj_f_projectile_pad_obj_2"
  ProjectilePadObj3           -> "obj_f_projectile_pad_obj_3"
  ProjectilePadIntArr1        -> "obj_f_projectile_pad_ias_1"
  ProjectilePadInt64Arr1      -> "obj_f_projectile_pad_i64as_1"
  ProjectilePadObjArr1        -> "obj_f_projectile_pad_objas_1"
  ProjectileEnd               -> "obj_f_projectile_end"

itemFieldName :: ItemField -> String
itemFieldName = \case
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

weaponFieldName :: WeaponField -> String
weaponFieldName = \case
  WeaponBegin           -> "obj_f_weapon_begin"
  WeaponFlags           -> "obj_f_weapon_flags"
  WeaponRange           -> "obj_f_weapon_range"
  WeaponAmmoType        -> "obj_f_weapon_ammo_type"
  WeaponAmmoConsumption -> "obj_f_weapon_ammo_consumption"
  WeaponMissileAid      -> "obj_f_weapon_missile_aid"
  WeaponCritHitChart    -> "obj_f_weapon_crit_hit_chart"
  WeaponAttacktype      -> "obj_f_weapon_attacktype"
  WeaponDamageDice      -> "obj_f_weapon_damage_dice"
  WeaponAnimtype        -> "obj_f_weapon_animtype"
  WeaponType            -> "obj_f_weapon_type"
  WeaponCritRange       -> "obj_f_weapon_crit_range"
  WeaponPadInt1         -> "obj_f_weapon_pad_i_1"
  WeaponPadInt2         -> "obj_f_weapon_pad_i_2"
  WeaponPadObj1         -> "obj_f_weapon_pad_obj_1"
  WeaponPadObj2         -> "obj_f_weapon_pad_obj_2"
  WeaponPadObj3         -> "obj_f_weapon_pad_obj_3"
  WeaponPadObj4         -> "obj_f_weapon_pad_obj_4"
  WeaponPadObj5         -> "obj_f_weapon_pad_obj_5"
  WeaponPadIntArr1      -> "obj_f_weapon_pad_ias_1"
  WeaponPadInt64Arr1    -> "obj_f_weapon_pad_i64as_1"
  WeaponEnd             -> "obj_f_weapon_end"

ammoFieldName :: AmmoField -> String
ammoFieldName = \case
  AmmoBegin        -> "obj_f_ammo_begin"
  AmmoFlags        -> "obj_f_ammo_flags"
  AmmoQuantity     -> "obj_f_ammo_quantity"
  AmmoType         -> "obj_f_ammo_type"
  AmmoPadInt1      -> "obj_f_ammo_pad_i_1"
  AmmoPadInt2      -> "obj_f_ammo_pad_i_2"
  AmmoPadObj1      -> "obj_f_ammo_pad_obj_1"
  AmmoPadIntArr1   -> "obj_f_ammo_pad_ias_1"
  AmmoPadInt64Arr1 -> "obj_f_ammo_pad_i64as_1"
  AmmoEnd          -> "obj_f_ammo_end"

armorFieldName :: ArmorField -> String
armorFieldName = \case
  ArmorBegin              -> "obj_f_armor_begin"
  ArmorFlags              -> "obj_f_armor_flags"
  ArmorAcAdj              -> "obj_f_armor_ac_adj"
  ArmorMaxDexBonus        -> "obj_f_armor_max_dex_bonus"
  ArmorArcaneSpellFailure -> "obj_f_armor_arcane_spell_failure"
  ArmorArmorCheckPenalty  -> "obj_f_armor_armor_check_penalty"
  ArmorPadInt1            -> "obj_f_armor_pad_i_1"
  ArmorPadIntArr1         -> "obj_f_armor_pad_ias_1"
  ArmorPadInt64Arr1       -> "obj_f_armor_pad_i64as_1"
  ArmorEnd                -> "obj_f_armor_end"

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

foodFieldName :: FoodField -> String
foodFieldName = \case
  FoodBegin        -> "obj_f_food_begin"
  FoodFlags        -> "obj_f_food_flags"
  FoodPadInt1      -> "obj_f_food_pad_i_1"
  FoodPadInt2      -> "obj_f_food_pad_i_2"
  FoodPadIntArr1   -> "obj_f_food_pad_ias_1"
  FoodPadInt64Arr1 -> "obj_f_food_pad_i64as_1"
  FoodEnd          -> "obj_f_food_end"

scrollFieldName :: ScrollField -> String
scrollFieldName = \case
  ScrollBegin        -> "obj_f_scroll_begin"
  ScrollFlags        -> "obj_f_scroll_flags"
  ScrollPadInt1      -> "obj_f_scroll_pad_i_1"
  ScrollPadInt2      -> "obj_f_scroll_pad_i_2"
  ScrollPadIntArr1   -> "obj_f_scroll_pad_ias_1"
  ScrollPadInt64Arr1 -> "obj_f_scroll_pad_i64as_1"
  ScrollEnd          -> "obj_f_scroll_end"

keyFieldName :: KeyField -> String
keyFieldName = \case
  KeyBegin        -> "obj_f_key_begin"
  KeyKeyId        -> "obj_f_key_key_id"
  KeyPadInt1      -> "obj_f_key_pad_i_1"
  KeyPadInt2      -> "obj_f_key_pad_i_2"
  KeyPadIntArr1   -> "obj_f_key_pad_ias_1"
  KeyPadInt64Arr1 -> "obj_f_key_pad_i64as_1"
  KeyEnd          -> "obj_f_key_end"

writtenFieldName :: WrittenField -> String
writtenFieldName = \case
  WrittenBegin         -> "obj_f_written_begin"
  WrittenFlags         -> "obj_f_written_flags"
  WrittenSubtype       -> "obj_f_written_subtype"
  WrittenTextStartLine -> "obj_f_written_text_start_line"
  WrittenTextEndLine   -> "obj_f_written_text_end_line"
  WrittenPadInt1       -> "obj_f_written_pad_i_1"
  WrittenPadInt2       -> "obj_f_written_pad_i_2"
  WrittenPadIntArr1    -> "obj_f_written_pad_ias_1"
  WrittenPadInt64Arr1  -> "obj_f_written_pad_i64as_1"
  WrittenEnd           -> "obj_f_written_end"

bagFieldName :: BagField -> String
bagFieldName = \case
  BagBegin -> "obj_f_bag_begin"
  BagFlags -> "obj_f_bag_flags"
  BagSize  -> "obj_f_bag_size"
  BagEnd   -> "obj_f_bag_end"

genericFieldName :: GenericField -> String
genericFieldName = \case
  GenericBegin               -> "obj_f_generic_begin"
  GenericFlags               -> "obj_f_generic_flags"
  GenericUsageBonus          -> "obj_f_generic_usage_bonus"
  GenericUsageCountRemaining -> "obj_f_generic_usage_count_remaining"
  GenericPadIntArr1          -> "obj_f_generic_pad_ias_1"
  GenericPadInt64Arr1        -> "obj_f_generic_pad_i64as_1"
  GenericEnd                 -> "obj_f_generic_end"

critterFieldName :: CritterField -> String
critterFieldName = \case
  CritterBegin                -> "obj_f_critter_begin"
  CritterFlags                -> "obj_f_critter_flags"
  CritterFlags2               -> "obj_f_critter_flags2"
  CritterAbilitiesIdx         -> "obj_f_critter_abilities_idx"
  CritterLevelIdx             -> "obj_f_critter_level_idx"
  CritterRace                 -> "obj_f_critter_race"
  CritterGender               -> "obj_f_critter_gender"
  CritterAge                  -> "obj_f_critter_age"
  CritterHeight               -> "obj_f_critter_height"
  CritterWeight               -> "obj_f_critter_weight"
  CritterExperience           -> "obj_f_critter_experience"
  CritterPadInt1              -> "obj_f_critter_pad_i_1"
  CritterAlignment            -> "obj_f_critter_alignment"
  CritterDeity                -> "obj_f_critter_deity"
  CritterDomain1              -> "obj_f_critter_domain_1"
  CritterDomain2              -> "obj_f_critter_domain_2"
  CritterAlignmentChoice      -> "obj_f_critter_alignment_choice"
  CritterSchoolSpecialization -> "obj_f_critter_school_specialization"
  CritterSpellsKnownIdx       -> "obj_f_critter_spells_known_idx"
  CritterSpellsMemorizedIdx   -> "obj_f_critter_spells_memorized_idx"
  CritterSpellsCastIdx        -> "obj_f_critter_spells_cast_idx"
  CritterFeatIdx              -> "obj_f_critter_feat_idx"
  CritterFeatCountIdx         -> "obj_f_critter_feat_count_idx"
  CritterFleeingFrom          -> "obj_f_critter_fleeing_from"
  CritterPortrait             -> "obj_f_critter_portrait"
  CritterMoneyIdx             -> "obj_f_critter_money_idx"
  CritterInventoryNum         -> "obj_f_critter_inventory_num"
  CritterInventoryListIdx     -> "obj_f_critter_inventory_list_idx"
  CritterInventorySource      -> "obj_f_critter_inventory_source"
  CritterDescriptionUnknown   -> "obj_f_critter_description_unknown"
  CritterFollowerIdx          -> "obj_f_critter_follower_idx"
  CritterTeleportDest         -> "obj_f_critter_teleport_dest"
  CritterTeleportMap          -> "obj_f_critter_teleport_map"
  CritterDeathTime            -> "obj_f_critter_death_time"
  CritterSkillIdx             -> "obj_f_critter_skill_idx"
  CritterReach                -> "obj_f_critter_reach"
  CritterSubdualDamage        -> "obj_f_critter_subdual_damage"
  CritterPadInt4              -> "obj_f_critter_pad_i_4"
  CritterPadInt5              -> "obj_f_critter_pad_i_5"
  CritterSequence             -> "obj_f_critter_sequence"
  CritterHairStyle            -> "obj_f_critter_hair_style"
  CritterStrategy             -> "obj_f_critter_strategy"
  CritterPadInt3              -> "obj_f_critter_pad_i_3"
  CritterMonsterCategory      -> "obj_f_critter_monster_category"
  CritterPadInt642            -> "obj_f_critter_pad_i64_2"
  CritterPadInt643            -> "obj_f_critter_pad_i64_3"
  CritterPadInt644            -> "obj_f_critter_pad_i64_4"
  CritterPadInt645            -> "obj_f_critter_pad_i64_5"
  CritterDamageIdx            -> "obj_f_critter_damage_idx"
  CritterAttacksIdx           -> "obj_f_critter_attacks_idx"
  CritterSeenMaplist          -> "obj_f_critter_seen_maplist"
  CritterPadInt64Arr2         -> "obj_f_critter_pad_i64as_2"
  CritterPadInt64Arr3         -> "obj_f_critter_pad_i64as_3"
  CritterPadInt64Arr4         -> "obj_f_critter_pad_i64as_4"
  CritterPadInt64Arr5         -> "obj_f_critter_pad_i64as_5"
  CritterEnd                  -> "obj_f_critter_end"

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

npcFieldName :: NpcField -> String
npcFieldName = \case
  NpcBegin                 -> "obj_f_npc_begin"
  NpcFlags                 -> "obj_f_npc_flags"
  NpcLeader                -> "obj_f_npc_leader"
  NpcAiData                -> "obj_f_npc_ai_data"
  NpcCombatFocus           -> "obj_f_npc_combat_focus"
  NpcWhoHitMeLast          -> "obj_f_npc_who_hit_me_last"
  NpcWaypointsIdx          -> "obj_f_npc_waypoints_idx"
  NpcWaypointCurrent       -> "obj_f_npc_waypoint_current"
  NpcFaction               -> "obj_f_npc_faction"
  NpcRetailPriceMultiplier -> "obj_f_npc_retail_price_multiplier"
  NpcSubstituteInventory   -> "obj_f_npc_substitute_inventory"
  NpcReactionBase          -> "obj_f_npc_reaction_base"
  NpcChallengeRating       -> "obj_f_npc_challenge_rating"
  NpcReactionPcIdx         -> "obj_f_npc_reaction_pc_idx"
  NpcReactionLevelIdx      -> "obj_f_npc_reaction_level_idx"
  NpcReactionTimeIdx       -> "obj_f_npc_reaction_time_idx"
  NpcGeneratorData         -> "obj_f_npc_generator_data"
  NpcAiListIdx             -> "obj_f_npc_ai_list_idx"
  NpcSaveReflexesBonus     -> "obj_f_npc_save_reflexes_bonus"
  NpcSaveFortitudeBonus    -> "obj_f_npc_save_fortitude_bonus"
  NpcSaveWillpowerBonus    -> "obj_f_npc_save_willpower_bonus"
  NpcAcBonus               -> "obj_f_npc_ac_bonus"
  NpcAddMesh               -> "obj_f_npc_add_mesh"
  NpcWaypointAnim          -> "obj_f_npc_waypoint_anim"
  NpcPadInt3               -> "obj_f_npc_pad_i_3"
  NpcPadInt4               -> "obj_f_npc_pad_i_4"
  NpcPadInt5               -> "obj_f_npc_pad_i_5"
  NpcAiFlags64             -> "obj_f_npc_ai_flags64"
  NpcPadInt642             -> "obj_f_npc_pad_i64_2"
  NpcPadInt643             -> "obj_f_npc_pad_i64_3"
  NpcPadInt644             -> "obj_f_npc_pad_i64_4"
  NpcPadInt645             -> "obj_f_npc_pad_i64_5"
  NpcHitdiceIdx            -> "obj_f_npc_hitdice_idx"
  NpcAiListTypeIdx         -> "obj_f_npc_ai_list_type_idx"
  NpcPadIntArr3            -> "obj_f_npc_pad_ias_3"
  NpcPadIntArr4            -> "obj_f_npc_pad_ias_4"
  NpcPadIntArr5            -> "obj_f_npc_pad_ias_5"
  NpcStandpoints           -> "obj_f_npc_standpoints"
  NpcPadInt64Arr2          -> "obj_f_npc_pad_i64as_2"
  NpcPadInt64Arr3          -> "obj_f_npc_pad_i64as_3"
  NpcPadInt64Arr4          -> "obj_f_npc_pad_i64as_4"
  NpcPadInt64Arr5          -> "obj_f_npc_pad_i64as_5"
  NpcEnd                   -> "obj_f_npc_end"

  NpcStandpointDayINVALID  ->
    "obj_f_npc_standpoint_day_INTERNAL_DO_NOT_USE"
  NpcStandpointNightINVALID ->
    "obj_f_npc_standpoint_night_INTERNAL_DO_NOT_USE"

trapFieldName :: TrapField -> String
trapFieldName = \case
  TrapBegin                -> "obj_f_trap_begin"
  TrapFlags                -> "obj_f_trap_flags"
  TrapDifficulty           -> "obj_f_trap_difficulty"
  TrapPadInt2              -> "obj_f_trap_pad_i_2"
  TrapPadIntArr1           -> "obj_f_trap_pad_ias_1"
  TrapPadInt64Arr1         -> "obj_f_trap_pad_i64as_1"
  TrapEnd                  -> "obj_f_trap_end"

extraFieldName :: ExtraField -> String
extraFieldName = \case
  TotalNormal         -> "obj_f_total_normal"
  TransientBegin      -> "obj_f_transient_begin"
  RenderColor         -> "obj_f_render_color"
  RenderColors        -> "obj_f_render_colors"
  RenderPalette       -> "obj_f_render_palette"
  RenderScale         -> "obj_f_render_scale"
  RenderAlpha         -> "obj_f_render_alpha"
  RenderX             -> "obj_f_render_x"
  RenderY             -> "obj_f_render_y"
  RenderWidth         -> "obj_f_render_width"
  RenderHeight        -> "obj_f_render_height"
  Palette             -> "obj_f_palette"
  Color               -> "obj_f_color"
  Colors              -> "obj_f_colors"
  RenderFlags         -> "obj_f_render_flags"
  TempId              -> "obj_f_temp_id"
  LightHandle         -> "obj_f_light_handle"
  OverlayLightHandles -> "obj_f_overlay_light_handles"
  InternalFlags       -> "obj_f_internal_flags"
  FindNode            -> "obj_f_find_node"
  AnimationHandle     -> "obj_f_animation_handle"
  GrappleState        -> "obj_f_grapple_state"
  TransientEnd        -> "obj_f_transient_end"
  Type                -> "obj_f_type"
  PrototypeHandle     -> "obj_f_prototype_handle"

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

generalFieldType :: GeneralField -> FieldType
generalFieldType = \case
  GeneralBegin -> BeginF
  Location -> LocF
  XOffset -> F32F
  YOffset -> F32F
  ShadowArt2D -> W32F
  BlitFlags -> W32F
  BlitColor -> W32F
  Transparency -> W32F
  ModelScale -> W32F
  LightFlags -> W32F
  LightMaterial -> W32F
  LightColor -> W32F
  LightRadius -> F32F
  LightAngleStart -> F32F
  LightAngleEnd -> F32F
  LightType -> W32F
  LightXFacing -> F32F
  LightYFacing -> F32F
  LightZFacing -> F32F
  LightXOffset -> F32F
  LightYOffset -> F32F
  LightZOffset -> F32F
  Flags -> W32F
  SpellFlags -> W32F
  Name -> W32F
  Description -> W32F
  Size -> W32F
  HpPts -> W32F
  HpAdj -> W32F
  HpDamage -> W32F
  Material -> W32F
  ScriptsIdx -> ScriptArrF
  SoundEffect -> W32F
  Category -> W32F
  Rotation -> F32F
  SpeedWalk -> F32F
  SpeedRun -> F32F
  BaseMesh -> W32F
  BaseAnim -> W32F
  Radius -> F32F
  RenderHeight3D -> F32F
  Conditions -> W32ArrF
  ConditionArg0 -> W32ArrF
  PermanentMods -> W32ArrF
  Initiative -> W32F
  Dispatcher -> B32F
  Subinitiative -> W32F
  SecretdoorFlags -> W32F
  SecretdoorEffectname -> W32F
  SecretdoorDC -> W32F
  PadInt7 -> W32F
  PadInt8 -> W32F
  PadInt9 -> W32F
  PadInt0 -> W32F
  ZOffset -> F32F
  RotationPitch -> F32F
  PadFloat3 -> F32F
  PadFloat4 -> F32F
  PadFloat5 -> F32F
  PadFloat6 -> F32F
  PadFloat7 -> F32F
  PadFloat8 -> F32F
  PadFloat9 -> F32F
  PadFloat0 -> F32F
  PadInt640 -> W64F
  PadInt641 -> W64F
  PadInt642 -> W64F
  PadInt643 -> W64F
  PadInt644 -> W64F
  LastHitBy -> ObjF
  PadObj1 -> ObjF
  PadObj2 -> ObjF
  PadObj3 -> ObjF
  PadObj4 -> ObjF
  PermanentModData -> W32ArrF
  AttackTypesIdx -> W32ArrF
  AttackBonusIdx -> W32ArrF
  StrategyState -> W32ArrF
  PadIntArr4 -> W32ArrF
  PadInt64Arr0 -> W64ArrF
  PadInt64Arr1 -> W64ArrF
  PadInt64Arr2 -> W64ArrF
  PadInt64Arr3 -> W64ArrF
  PadInt64Arr4 -> W64ArrF
  PadObjArr0 -> ObjArrF
  PadObjArr1 -> ObjArrF
  PadObjArr2 -> ObjArrF
  GeneralEnd -> EndF

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

projectileFieldType :: ProjectileField -> FieldType
projectileFieldType = \case
  ProjectileBegin -> BeginF
  ProjectileFlagsCombat -> W32F
  ProjectileFlagsCombatDamage -> W32F
  ProjectileParentWeapon -> ObjF
  ProjectileParentAmmo -> ObjF
  ProjectilePartSysId -> W32F
  ProjectileAccelerationX -> F32F
  ProjectileAccelerationY -> F32F
  ProjectileAccelerationZ -> F32F
  ProjectilePadInt4 -> W32F
  ProjectilePadObj1 -> ObjF
  ProjectilePadObj2 -> ObjF
  ProjectilePadObj3 -> ObjF
  ProjectilePadIntArr1 -> W32ArrF
  ProjectilePadInt64Arr1 -> W64ArrF
  ProjectilePadObjArr1 -> ObjArrF
  ProjectileEnd -> EndF

itemFieldType :: ItemField -> FieldType
itemFieldType = \case
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

weaponFieldType :: WeaponField -> FieldType
weaponFieldType = \case
  WeaponBegin -> BeginF
  WeaponFlags -> W32F
  WeaponRange -> W32F
  WeaponAmmoType -> W32F
  WeaponAmmoConsumption -> W32F
  WeaponMissileAid -> W32F
  WeaponCritHitChart -> W32F
  WeaponAttacktype -> W32F
  WeaponDamageDice -> W32F
  WeaponAnimtype -> W32F
  WeaponType -> W32F
  WeaponCritRange -> W32F
  WeaponPadInt1 -> W32F
  WeaponPadInt2 -> W32F
  WeaponPadObj1 -> ObjF
  WeaponPadObj2 -> ObjF
  WeaponPadObj3 -> ObjF
  WeaponPadObj4 -> ObjF
  WeaponPadObj5 -> ObjF
  WeaponPadIntArr1 -> W32ArrF
  WeaponPadInt64Arr1 -> W64ArrF
  WeaponEnd -> EndF

ammoFieldType :: AmmoField -> FieldType
ammoFieldType = \case
  AmmoBegin -> BeginF
  AmmoFlags -> W32F
  AmmoQuantity -> W32F
  AmmoType -> W32F
  AmmoPadInt1 -> W32F
  AmmoPadInt2 -> W32F
  AmmoPadObj1 -> ObjF
  AmmoPadIntArr1 -> W32ArrF
  AmmoPadInt64Arr1 -> W64ArrF
  AmmoEnd -> EndF

armorFieldType :: ArmorField -> FieldType
armorFieldType = \case
  ArmorBegin -> BeginF
  ArmorFlags -> W32F
  ArmorAcAdj -> W32F
  ArmorMaxDexBonus -> W32F
  ArmorArcaneSpellFailure -> W32F
  ArmorArmorCheckPenalty -> W32F
  ArmorPadInt1 -> W32F
  ArmorPadIntArr1 -> W32ArrF
  ArmorPadInt64Arr1 -> W64ArrF
  ArmorEnd -> EndF

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

foodFieldType :: FoodField -> FieldType
foodFieldType = \case
  FoodBegin -> BeginF
  FoodFlags -> W32F
  FoodPadInt1 -> W32F
  FoodPadInt2 -> W32F
  FoodPadIntArr1 -> W32ArrF
  FoodPadInt64Arr1 -> W64ArrF
  FoodEnd -> EndF

scrollFieldType :: ScrollField -> FieldType
scrollFieldType = \case
  ScrollBegin -> BeginF
  ScrollFlags -> W32F
  ScrollPadInt1 -> W32F
  ScrollPadInt2 -> W32F
  ScrollPadIntArr1 -> W32ArrF
  ScrollPadInt64Arr1 -> W64ArrF
  ScrollEnd -> EndF

keyFieldType :: KeyField -> FieldType
keyFieldType = \case
  KeyBegin -> BeginF
  KeyKeyId -> W32F
  KeyPadInt1 -> W32F
  KeyPadInt2 -> W32F
  KeyPadIntArr1 -> W32ArrF
  KeyPadInt64Arr1 -> W64ArrF
  KeyEnd -> EndF

writtenFieldType :: WrittenField -> FieldType
writtenFieldType = \case
  WrittenBegin -> BeginF
  WrittenFlags -> W32F
  WrittenSubtype -> W32F
  WrittenTextStartLine -> W32F
  WrittenTextEndLine -> W32F
  WrittenPadInt1 -> W32F
  WrittenPadInt2 -> W32F
  WrittenPadIntArr1 -> W32ArrF
  WrittenPadInt64Arr1 -> W64ArrF
  WrittenEnd -> EndF

bagFieldType :: BagField -> FieldType
bagFieldType = \case
  BagBegin -> BeginF
  BagFlags -> W32F
  BagSize -> W32F
  BagEnd -> EndF

genericFieldType :: GenericField -> FieldType
genericFieldType = \case
  GenericBegin -> BeginF
  GenericFlags -> W32F
  GenericUsageBonus -> W32F
  GenericUsageCountRemaining -> W32F
  GenericPadIntArr1 -> W32ArrF
  GenericPadInt64Arr1 -> W64ArrF
  GenericEnd -> EndF

critterFieldType :: CritterField -> FieldType
critterFieldType = \case
  CritterBegin -> BeginF
  CritterFlags -> W32F
  CritterFlags2 -> W32F
  CritterAbilitiesIdx -> AbilityArrF
  CritterLevelIdx -> W32ArrF
  CritterRace -> W32F
  CritterGender -> W32F
  CritterAge -> W32F
  CritterHeight -> W32F
  CritterWeight -> W32F
  CritterExperience -> W32F
  CritterPadInt1 -> W32F
  CritterAlignment -> W32F
  CritterDeity -> W32F
  CritterDomain1 -> W32F
  CritterDomain2 -> W32F
  CritterAlignmentChoice -> W32F
  CritterSchoolSpecialization -> W32F
  CritterSpellsKnownIdx -> SpellArrF
  CritterSpellsMemorizedIdx -> SpellArrF
  CritterSpellsCastIdx -> SpellArrF
  CritterFeatIdx -> W32ArrF
  CritterFeatCountIdx -> W32ArrF
  CritterFleeingFrom -> ObjF
  CritterPortrait -> W32F
  CritterMoneyIdx -> W32ArrF
  CritterInventoryNum -> W32F
  CritterInventoryListIdx -> ObjArrF
  CritterInventorySource -> W32F
  CritterDescriptionUnknown -> W32F
  CritterFollowerIdx -> ObjArrF
  CritterTeleportDest -> LocF
  CritterTeleportMap -> W32F
  CritterDeathTime -> W32F
  CritterSkillIdx -> W32ArrF
  CritterReach -> W32F
  CritterSubdualDamage -> W32F
  CritterPadInt4 -> W32F
  CritterPadInt5 -> W32F
  CritterSequence -> W32F
  CritterHairStyle -> W32F
  CritterStrategy -> W32F
  CritterPadInt3 -> W32F
  CritterMonsterCategory -> W64F
  CritterPadInt642 -> W64F
  CritterPadInt643 -> W64F
  CritterPadInt644 -> W64F
  CritterPadInt645 -> W64F
  CritterDamageIdx -> W32ArrF
  CritterAttacksIdx -> W32ArrF
  CritterSeenMaplist -> W64ArrF
  CritterPadInt64Arr2 -> W64ArrF
  CritterPadInt64Arr3 -> W64ArrF
  CritterPadInt64Arr4 -> W64ArrF
  CritterPadInt64Arr5 -> W64ArrF
  CritterEnd -> EndF

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

npcFieldType :: NpcField -> FieldType
npcFieldType = \case
  NpcBegin -> BeginF
  NpcFlags -> W32F
  NpcLeader -> ObjF
  NpcAiData -> W32F
  NpcCombatFocus -> ObjF
  NpcWhoHitMeLast -> ObjF
  NpcWaypointsIdx -> WayptArrF
  NpcWaypointCurrent -> W32F
  NpcStandpointDayINVALID -> LocF
  NpcStandpointNightINVALID -> LocF
  NpcFaction -> W32ArrF
  NpcRetailPriceMultiplier -> W32F
  NpcSubstituteInventory -> ObjF
  NpcReactionBase -> W32F
  NpcChallengeRating -> W32F
  NpcReactionPcIdx -> ObjArrF
  NpcReactionLevelIdx -> W32ArrF
  NpcReactionTimeIdx -> W32ArrF
  NpcGeneratorData -> W32F
  NpcAiListIdx -> ObjArrF
  NpcSaveReflexesBonus -> W32F
  NpcSaveFortitudeBonus -> W32F
  NpcSaveWillpowerBonus -> W32F
  NpcAcBonus -> W32F
  NpcAddMesh -> W32F
  NpcWaypointAnim -> W32F
  NpcPadInt3 -> W32F
  NpcPadInt4 -> W32F
  NpcPadInt5 -> W32F
  NpcAiFlags64 -> W64F
  NpcPadInt642 -> W64F
  NpcPadInt643 -> W64F
  NpcPadInt644 -> W64F
  NpcPadInt645 -> W64F
  NpcHitdiceIdx -> W32ArrF
  NpcAiListTypeIdx -> W32ArrF
  NpcPadIntArr3 -> W32ArrF
  NpcPadIntArr4 -> W32ArrF
  NpcPadIntArr5 -> W32ArrF
  NpcStandpoints -> StandptArrF
  NpcPadInt64Arr2 -> W64ArrF
  NpcPadInt64Arr3 -> W64ArrF
  NpcPadInt64Arr4 -> W64ArrF
  NpcPadInt64Arr5 -> W64ArrF
  NpcEnd -> EndF

trapFieldType :: TrapField -> FieldType
trapFieldType = \case
  TrapBegin -> BeginF
  TrapFlags -> W32F
  TrapDifficulty -> W32F
  TrapPadInt2 -> W32F
  TrapPadIntArr1 -> W32ArrF
  TrapPadInt64Arr1 -> W64ArrF
  TrapEnd -> EndF

extraFieldType :: ExtraField -> FieldType
extraFieldType = \case
  TotalNormal -> NoneF
  TransientBegin -> BeginF
  RenderColor -> W32F
  RenderColors -> W32F
  RenderPalette -> W32F
  RenderScale -> W32F
  RenderAlpha -> AbilityArrF
  RenderX -> W32F
  RenderY -> W32F
  RenderWidth -> W32F
  RenderHeight -> W32F
  Palette -> W32F
  Color -> W32F
  Colors -> W32F
  RenderFlags -> W32F
  TempId -> W32F
  LightHandle -> W32F
  OverlayLightHandles -> W32ArrF
  InternalFlags -> W32F
  FindNode -> W32F
  AnimationHandle -> W32F
  GrappleState -> W32F
  TransientEnd -> EndF
  Type -> W32F
  PrototypeHandle -> ObjF

objectScriptName :: ObjectScript -> String
objectScriptName = \case
  SanExamine         -> "san_examine"
  SanUse             -> "san_use"
  SanDestroy         -> "san_destroy"
  SanUnlock          -> "san_unlock"
  SanGet             -> "san_get"
  SanDrop            -> "san_drop"
  SanThrow           -> "san_throw"
  SanHit             -> "san_hit"
  SanMiss            -> "san_miss"
  SanDialog          -> "san_dialog"
  SanFirstHeartbeat  -> "san_first_heartbeat"
  SanCatchingThiefPc -> "san_catching_thief_pc"
  SanDying           -> "san_dying"
  SanEnterCombat     -> "san_enter_combat"
  SanExitCombat      -> "san_exit_combat"
  SanStartCombat     -> "san_start_combat"
  SanEndCombat       -> "san_end_combat"
  SanBuyObject       -> "san_buy_object"
  SanResurrect       -> "san_resurrect"
  SanHeartbeat       -> "san_heartbeat"
  SanLeaderKilling   -> "san_leader_killing"
  SanInsertItem      -> "san_insert_item"
  SanWillKos         -> "san_will_kos"
  SanTakingDamage    -> "san_taking_damage"
  SanWieldOn         -> "san_wield_on"
  SanWieldOff        -> "san_wield_off"
  SanCritterHits     -> "san_critter_hits"
  SanNewSector       -> "san_new_sector"
  SanRemoveItem      -> "san_remove_item"
  SanLeaderSleeping  -> "san_leader_sleeping"
  SanBust            -> "san_bust"
  SanDialogOverride  -> "san_dialog_override"
  SanTransfer        -> "san_transfer"
  SanCaughtThief     -> "san_caught_thief"
  SanCriticalHit     -> "san_critical_hit"
  SanCriticalMiss    -> "san_critical_miss"
  SanJoin            -> "san_join"
  SanDisband         -> "san_disband"
  SanNewMap          -> "san_new_map"
  SanTrap            -> "san_trap"
  SanTrueSeeing      -> "san_true_seeing"
  SanSpellCast       -> "san_spell_cast"
  SanUnlockAttempt   -> "san_unlock_attempt"

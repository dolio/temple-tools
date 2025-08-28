
module Temple.Object.Field.Critter where

import Data.String
import Text.Megaparsec

import Temple.Object.Field.Type

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

  enumFrom n = enumFromTo n maxBound
  enumFromThen m n = enumFromThenTo m n maxBound

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
  CritterSkillIdx -> SkillArrF
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

parsePartialCritterFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m CritterField
parsePartialCritterFieldName =
  choice
    [ CritterBegin                <$ chunk "begin"
    , CritterFlags                <$ chunk "flags"
    , CritterFlags2               <$ chunk "flags2"
    , CritterAbilitiesIdx         <$ chunk "abilities_idx"
    , CritterLevelIdx             <$ chunk "level_idx"
    , CritterRace                 <$ chunk "race"
    , CritterGender               <$ chunk "gender"
    , CritterAge                  <$ chunk "age"
    , CritterHeight               <$ chunk "height"
    , CritterWeight               <$ chunk "weight"
    , CritterExperience           <$ chunk "experience"
    , CritterPadInt1              <$ chunk "pad_i_1"
    , CritterAlignment            <$ chunk "alignment"
    , CritterDeity                <$ chunk "deity"
    , CritterDomain1              <$ chunk "domain_1"
    , CritterDomain2              <$ chunk "domain_2"
    , CritterAlignmentChoice      <$ chunk "alignment_choice"
    , CritterSchoolSpecialization <$ chunk "school_specialization"
    , CritterSpellsKnownIdx       <$ chunk "spells_known_idx"
    , CritterSpellsMemorizedIdx   <$ chunk "spells_memorized_idx"
    , CritterSpellsCastIdx        <$ chunk "spells_cast_idx"
    , CritterFeatIdx              <$ chunk "feat_idx"
    , CritterFeatCountIdx         <$ chunk "feat_count_idx"
    , CritterFleeingFrom          <$ chunk "fleeing_from"
    , CritterPortrait             <$ chunk "portrait"
    , CritterMoneyIdx             <$ chunk "money_idx"
    , CritterInventoryNum         <$ chunk "inventory_num"
    , CritterInventoryListIdx     <$ chunk "inventory_list_idx"
    , CritterInventorySource      <$ chunk "inventory_source"
    , CritterDescriptionUnknown   <$ chunk "description_unknown"
    , CritterFollowerIdx          <$ chunk "follower_idx"
    , CritterTeleportDest         <$ chunk "teleport_dest"
    , CritterTeleportMap          <$ chunk "teleport_map"
    , CritterDeathTime            <$ chunk "death_time"
    , CritterSkillIdx             <$ chunk "skill_idx"
    , CritterReach                <$ chunk "reach"
    , CritterSubdualDamage        <$ chunk "subdual_damage"
    , CritterPadInt4              <$ chunk "pad_i_4"
    , CritterPadInt5              <$ chunk "pad_i_5"
    , CritterSequence             <$ chunk "sequence"
    , CritterHairStyle            <$ chunk "hair_style"
    , CritterStrategy             <$ chunk "strategy"
    , CritterPadInt3              <$ chunk "pad_i_3"
    , CritterMonsterCategory      <$ chunk "monster_category"
    , CritterPadInt642            <$ chunk "pad_i64_2"
    , CritterPadInt643            <$ chunk "pad_i64_3"
    , CritterPadInt644            <$ chunk "pad_i64_4"
    , CritterPadInt645            <$ chunk "pad_i64_5"
    , CritterDamageIdx            <$ chunk "damage_idx"
    , CritterAttacksIdx           <$ chunk "attacks_idx"
    , CritterSeenMaplist          <$ chunk "seen_maplist"
    , CritterPadInt64Arr2         <$ chunk "pad_i64as_2"
    , CritterPadInt64Arr3         <$ chunk "pad_i64as_3"
    , CritterPadInt64Arr4         <$ chunk "pad_i64as_4"
    , CritterPadInt64Arr5         <$ chunk "pad_i64as_5"
    , CritterEnd                  <$ chunk "end"
    ]

parseCritterFieldName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m CritterField
parseCritterFieldName = chunk "obj_f_critter_" *> parsePartialCritterFieldName


module Temple.Object.Field.Npc where

import Temple.Object.Field.Type

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



module Temple.Object.Script where

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

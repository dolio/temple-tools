
module Temple.Object.Script where

import Data.String
import Text.Megaparsec

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

parseScriptName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m ObjectScript
parseScriptName = chunk "san_" *>
  choice
    [ SanExamine         <$ chunk "examine"
    , SanUse             <$ chunk "use"
    , SanDestroy         <$ chunk "destroy"
    , SanUnlock          <$ chunk "unlock"
    , SanGet             <$ chunk "get"
    , SanDrop            <$ chunk "drop"
    , SanThrow           <$ chunk "throw"
    , SanHit             <$ chunk "hit"
    , SanMiss            <$ chunk "miss"
    , SanDialog          <$ chunk "dialog"
    , SanFirstHeartbeat  <$ chunk "first_heartbeat"
    , SanCatchingThiefPc <$ chunk "catching_thief_pc"
    , SanDying           <$ chunk "dying"
    , SanEnterCombat     <$ chunk "enter_combat"
    , SanExitCombat      <$ chunk "exit_combat"
    , SanStartCombat     <$ chunk "start_combat"
    , SanEndCombat       <$ chunk "end_combat"
    , SanBuyObject       <$ chunk "buy_object"
    , SanResurrect       <$ chunk "resurrect"
    , SanHeartbeat       <$ chunk "heartbeat"
    , SanLeaderKilling   <$ chunk "leader_killing"
    , SanInsertItem      <$ chunk "insert_item"
    , SanWillKos         <$ chunk "will_kos"
    , SanTakingDamage    <$ chunk "taking_damage"
    , SanWieldOn         <$ chunk "wield_on"
    , SanWieldOff        <$ chunk "wield_off"
    , SanCritterHits     <$ chunk "critter_hits"
    , SanNewSector       <$ chunk "new_sector"
    , SanRemoveItem      <$ chunk "remove_item"
    , SanLeaderSleeping  <$ chunk "leader_sleeping"
    , SanBust            <$ chunk "bust"
    , SanDialogOverride  <$ chunk "dialog_override"
    , SanTransfer        <$ chunk "transfer"
    , SanCaughtThief     <$ chunk "caught_thief"
    , SanCriticalHit     <$ chunk "critical_hit"
    , SanCriticalMiss    <$ chunk "critical_miss"
    , SanJoin            <$ chunk "join"
    , SanDisband         <$ chunk "disband"
    , SanNewMap          <$ chunk "new_map"
    , SanTrap            <$ chunk "trap"
    , SanTrueSeeing      <$ chunk "true_seeing"
    , SanSpellCast       <$ chunk "spell_cast"
    , SanUnlockAttempt   <$ chunk "unlock_attempt"
    ]

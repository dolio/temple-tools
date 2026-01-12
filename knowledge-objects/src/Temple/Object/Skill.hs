
module Temple.Object.Skill where

import Data.String
import Text.Megaparsec

-- Skills in order according to ToEE. `Enum` instance the enumeration used in
-- the actual game. Several are not actually used/enabled in the default game.
data Skill
  = Appraise
  | Bluff
  | Concentration
  | Diplomacy
  | DisableDevice
  | GatherInformation
  | Heal
  | Hide
  | Intimidate
  | Listen
  | MoveSilently
  | OpenLock
  | PickPocket
  | Search
  | SenseMotive
  | Spellcraft
  | Spot
  | Tumble
  | UseMagicDevice
  | Survival
  | Perform
  | Alchemy
  | Balance
  | Climb
  | Craft
  | DecipherScript
  | Disguise
  | EscapeArtist
  | Forgery
  | HandleAnimal
  | Innuendo
  | IntuitDirection
  | Jump
  | KnowledgeArcana
  | KnowledgeReligion
  | KnowledgeNature
  | KnowledgeAll
  | Profession
  | ReadLips
  | Ride
  | Swim
  | UseRope
  deriving (Bounded, Enum, Eq, Ord, Show)

skillName :: IsString s => Skill -> s
skillName = \case
  Appraise          -> "skill_appraise"
  Bluff             -> "skill_bluff"
  Concentration     -> "skill_concentration"
  Diplomacy         -> "skill_diplomacy"
  DisableDevice     -> "skill_disable_device"
  GatherInformation -> "skill_gather_information"
  Heal              -> "skill_heal"
  Hide              -> "skill_hide"
  Intimidate        -> "skill_intimidate"
  Listen            -> "skill_listen"
  MoveSilently      -> "skill_move_silently"
  OpenLock          -> "skill_open_lock"
  PickPocket        -> "skill_pick_pocket"
  Search            -> "skill_search"
  SenseMotive       -> "skill_sense_motive"
  Spellcraft        -> "skill_spellcraft"
  Spot              -> "skill_spot"
  Tumble            -> "skill_tumble"
  UseMagicDevice    -> "skill_use_magic_device"
  Survival          -> "skill_survival"
  Perform           -> "skill_perform"
  Alchemy           -> "skill_alchemy"
  Balance           -> "skill_balance"
  Climb             -> "skill_climb"
  Craft             -> "skill_craft"
  DecipherScript    -> "skill_decipher_script"
  Disguise          -> "skill_disguise"
  EscapeArtist      -> "skill_escape_artist"
  Forgery           -> "skill_forgery"
  HandleAnimal      -> "skill_handle_animal"
  Innuendo          -> "skill_innuendo"
  IntuitDirection   -> "skill_intuit_direction"
  Jump              -> "skill_jump"
  KnowledgeArcana   -> "skill_knowledge_arcana"
  KnowledgeReligion -> "skill_knowledge_religion"
  KnowledgeNature   -> "skill_knowledge_nature"
  KnowledgeAll      -> "skill_knowledge_all"
  Profession        -> "skill_profession"
  ReadLips          -> "skill_read_lips"
  Ride              -> "skill_ride"
  Swim              -> "skill_swim"
  UseRope           -> "skill_use_rope"
{-# inlinable skillName #-}

parseSkillName
  :: MonadParsec e s m
  => IsString (Tokens s)
  => m Skill
parseSkillName = chunk "skill_" *>
  choice
    [ Appraise          <$ chunk "appraise"
    , Bluff             <$ chunk "bluff"
    , Concentration     <$ chunk "concentration"
    , Diplomacy         <$ chunk "diplomacy"
    , DisableDevice     <$ chunk "disable_device"
    , GatherInformation <$ chunk "gather_information"
    , Heal              <$ chunk "heal"
    , Hide              <$ chunk "hide"
    , Intimidate        <$ chunk "intimidate"
    , Listen            <$ chunk "listen"
    , MoveSilently      <$ chunk "move_silently"
    , OpenLock          <$ chunk "open_lock"
    , PickPocket        <$ chunk "pick_pocket"
    , Search            <$ chunk "search"
    , SenseMotive       <$ chunk "sense_motive"
    , Spellcraft        <$ chunk "spellcraft"
    , Spot              <$ chunk "spot"
    , Tumble            <$ chunk "tumble"
    , UseMagicDevice    <$ chunk "use_magic_device"
    , Survival          <$ chunk "survival"
    , Perform           <$ chunk "perform"
    , Alchemy           <$ chunk "alchemy"
    , Balance           <$ chunk "balance"
    , Climb             <$ chunk "climb"
    , Craft             <$ chunk "craft"
    , DecipherScript    <$ chunk "decipher_script"
    , Disguise          <$ chunk "disguise"
    , EscapeArtist      <$ chunk "escape_artist"
    , Forgery           <$ chunk "forgery"
    , HandleAnimal      <$ chunk "handle_animal"
    , Innuendo          <$ chunk "innuendo"
    , IntuitDirection   <$ chunk "intuit_direction"
    , Jump              <$ chunk "jump"
    , KnowledgeArcana   <$ chunk "knowledge_arcana"
    , KnowledgeReligion <$ chunk "knowledge_religion"
    , KnowledgeNature   <$ chunk "knowledge_nature"
    , KnowledgeAll      <$ chunk "knowledge_all"
    , Profession        <$ chunk "profession"
    , ReadLips          <$ chunk "read_lips"
    , Ride              <$ chunk "ride"
    , Swim              <$ chunk "swim"
    , UseRope           <$ chunk "use_rope"
    ]

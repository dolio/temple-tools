
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
    [ Appraise          <$ chunk "skill_appraise"
    , Bluff             <$ chunk "skill_bluff"
    , Concentration     <$ chunk "skill_concentration"
    , Diplomacy         <$ chunk "skill_diplomacy"
    , DisableDevice     <$ chunk "skill_disable_device"
    , GatherInformation <$ chunk "skill_gather_information"
    , Heal              <$ chunk "skill_heal"
    , Hide              <$ chunk "skill_hide"
    , Intimidate        <$ chunk "skill_intimidate"
    , Listen            <$ chunk "skill_listen"
    , MoveSilently      <$ chunk "skill_move_silently"
    , OpenLock          <$ chunk "skill_open_lock"
    , PickPocket        <$ chunk "skill_pick_pocket"
    , Search            <$ chunk "skill_search"
    , SenseMotive       <$ chunk "skill_sense_motive"
    , Spellcraft        <$ chunk "skill_spellcraft"
    , Spot              <$ chunk "skill_spot"
    , Tumble            <$ chunk "skill_tumble"
    , UseMagicDevice    <$ chunk "skill_use_magic_device"
    , Survival          <$ chunk "skill_survival"
    , Perform           <$ chunk "skill_perform"
    , Alchemy           <$ chunk "skill_alchemy"
    , Balance           <$ chunk "skill_balance"
    , Climb             <$ chunk "skill_climb"
    , Craft             <$ chunk "skill_craft"
    , DecipherScript    <$ chunk "skill_decipher_script"
    , Disguise          <$ chunk "skill_disguise"
    , EscapeArtist      <$ chunk "skill_escape_artist"
    , Forgery           <$ chunk "skill_forgery"
    , HandleAnimal      <$ chunk "skill_handle_animal"
    , Innuendo          <$ chunk "skill_innuendo"
    , IntuitDirection   <$ chunk "skill_intuit_direction"
    , Jump              <$ chunk "skill_jump"
    , KnowledgeArcana   <$ chunk "skill_knowledge_arcana"
    , KnowledgeReligion <$ chunk "skill_knowledge_religion"
    , KnowledgeNature   <$ chunk "skill_knowledge_nature"
    , KnowledgeAll      <$ chunk "skill_knowledge_all"
    , Profession        <$ chunk "skill_profession"
    , ReadLips          <$ chunk "skill_read_lips"
    , Ride              <$ chunk "skill_ride"
    , Swim              <$ chunk "skill_swim"
    , UseRope           <$ chunk "skill_use_rope"
    ]

module Main (main) where

import Control.Monad (when)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Builder qualified as Bu
import Data.Char (toLower)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.UUID (UUID)
import Data.Word
import System.Directory
import System.IO as IO
import System.Exit
import System.FilePath
import Text.Read (readMaybe)

import Options.Applicative

import Temple.Object.Field

import Condition
import Decode
import Encode
import Display
import Mob
import Parse

data Action
  = Mob2Json
    { _jsonOut :: Maybe FilePath
    , _mobIn   :: FilePath
    }
  | Mob2Mob
    { _mobIn  :: FilePath
    , _mobOut :: FilePath
    }
  | AnalyzeMob
    { aopts :: AnalyzeOpts
    , mobIn :: FilePath
    }
  | Mdy2Mobs
    { saveIn :: FilePath
    , dirOut :: FilePath
    }
  | Md2Json
    { mapDir :: Maybe FilePath
    , saveIn :: FilePath
    , dirOut :: FilePath
    }
  | Json2Mob
    { _mmobOut :: Maybe FilePath
    , _jsonIn  :: FilePath
    }
  | Player2Json
    { jsonOut :: Maybe FilePath
    , pcIn    :: FilePath
    }

data AnalyzeOpts
  = AO
  { quietFailure  :: Bool
  , reportSuccess :: Bool
  , unknownConds  :: Bool
  , searchProto   :: Maybe Word32
  , searchCond    :: [String]
  }

inputArg :: Parser FilePath
inputArg
  = strArgument
  $ metavar "INPUT_FILE"
 <> action "file"

outputArg :: Parser FilePath
outputArg
  = strArgument
  $ metavar "OUPUT_FILE"
 <> action "file"

outputDirArg :: Parser FilePath
outputDirArg
  = strArgument
  $ metavar "OUTPUT_DIRECTORY"
 <> action "directory"

outputOpt :: Parser (Maybe FilePath)
outputOpt = option (maybeReader $ Just . Just)
          $ long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "Output file"
         <> value Nothing
         <> action "file"

mapDirOpt :: Parser (Maybe FilePath)
mapDirOpt = option (maybeReader $ Just . Just)
          $ long "map-dir"
         <> metavar "DIR"
         <> help "location of .mob files"
         <> value Nothing
         <> action "directory"

mob2json :: Mod CommandFields Action
mob2json = command "mob-to-json" $ info cmd desc where
  cmd = Mob2Json <$> outputOpt <*> inputArg
  desc = progDesc "Turn a MOB file into (somewhat) readable JSON"

mob2mob :: Mod CommandFields Action
mob2mob = command "mob-to-mob" $ info cmd desc where
  cmd = Mob2Mob <$> inputArg <*> outputArg
  desc = progDesc "Parse and emit MOB files to check relative idempotence"

json2mob :: Mod CommandFields Action
json2mob = command "json-to-mob" $ info cmd desc where
  cmd = Json2Mob <$> outputOpt <*> inputArg
  desc = progDesc "Turn JSON bac into a MOB file"

mdy2mobs :: Mod CommandFields Action
mdy2mobs = command "mdy-to-mobs" $ info cmd desc where
  cmd = Mdy2Mobs <$> inputArg <*> outputDirArg
  desc = progDesc "Turn a mobile.mdy into individual mobs"

md2json :: Mod CommandFields Action
md2json = command "md-to-json" $ info cmd desc where
  cmd = Md2Json <$> mapDirOpt <*> inputArg <*> outputDirArg
  desc = progDesc "Turn a mobile.md into readable files"

player2json :: Mod CommandFields Action
player2json = command "player-to-json" $ info cmd desc where
  cmd = Player2Json <$> outputOpt <*> inputArg
  desc = progDesc "Turn a .ToEEPC file into a readable format"

analyzeOpts :: Parser AnalyzeOpts
analyzeOpts = AO <$> failure <*> success <*> unknown <*> proto <*> many cond
  where
  failure = switch $ long "quiet-failure" <> short 'F'
  success = switch $ long "report-success" <> short 's'
  unknown = switch
          $ long "unknown-conditions"
         <> short 'C'
         <> help "Print unrecognized condition ids"
  proto = option (maybeReader $ fmap Just . readMaybe)
        $ long "proto"
       <> metavar "PROTO_ID"
       <> help "Print if prototype id matches"
       <> value Nothing
  cond = strOption
       $ long "has-condition"
      <> short 'c'
      <> help "Print if mob has the condition"

analyzeMob :: Mod CommandFields Action
analyzeMob = command "analyze-mob" $ info cmd desc where
  cmd = AnalyzeMob <$> analyzeOpts <*> inputArg
  desc = progDesc "Try to decode a MOB file and check information about it"

acts :: ParserInfo Action
acts = info (subs <**> helper) desc where
  subs = hsubparser
       $ mob2json
      <> mob2mob
      <> json2mob
      <> analyzeMob
      <> mdy2mobs
      <> md2json
      <> player2json
  desc = progDesc "manipulate various ToEE MOB representations"

main :: IO ()
main = customExecParser p acts >>= \case
  Mob2Json mout mobFile ->
    decodeMobOrFail False mobFile >>= \mob -> do
      condNames <- readConditionFile
      Bu.writeFile out $ displayMob condNames mob
      exitWith ExitSuccess
    where
    out = fromMaybe (mobFile <.> "json") mout
  Mob2Mob mobIn mobOut ->
    decodeMobOrFail False mobIn >>= L.writeFile mobOut . encodeMob
  Json2Mob mout jin ->
    parseJsonOrFail jin >>= L.writeFile mobOut . encodeMob
    where
    mobOut = fromMaybe (dropExtension jin <.> "mob") mout
  AnalyzeMob {..} -> do
    decodeMobOrFail (quietFailure aopts) mobIn >>=
      performAnalysis mobIn aopts
    exitWith ExitSuccess
  Mdy2Mobs {..} -> decodeMobsOrFail saveIn >>= \idms -> do
    createDirectoryIfMissing True dirOut
    withCurrentDirectory dirOut $
      for_ idms \mob ->
        L.writeFile (objectIdToFileName (objId mob) <.> "mob") $ encodeMob mob
  Md2Json {..} -> do
    mobs <- loadMobsFromDirectory mobLoc
    diffs <- decodeDiffsOrFail mobs saveIn
    createDirectoryIfMissing True dirOut
    condNames <- readConditionFile
    withCurrentDirectory dirOut $
      for_ diffs \(obId, diff) ->
        Bu.writeFile (objectIdToFileName obId <.> "json") $
          displayDiff condNames diff
    where
    mobLoc = fromMaybe (dropFileName saveIn) mapDir
  Player2Json {..} -> do
    plr <- decodePlayerOrFail pcIn
    condNames <- readConditionFile
    Bu.writeFile out $ displayPlayer condNames plr
    where
    out = fromMaybe (pcIn <.> "json") jsonOut
  where
  p = prefs showHelpOnEmpty

performAnalysis :: FilePath -> AnalyzeOpts -> Mob -> IO ()
performAnalysis fname (AO {..}) mob = do
  when (not . checkUUID fname $ objId mob) do
    when (not quietFailure) do
      IO.hPutStr stderr fname
      hPutStrLn stderr ": UUID mismatch"

  when reportSuccess do
    IO.putStr fname
    putStrLn " OK"

  when (not $ null searchCond) $
    let match = testConds searchCond mob
     in when (not $ null match) do
          IO.putStr fname
          IO.putStr " condition(s) matched: "
          print match

  when unknownConds do
    condNames <- readConditionFile
    let unk = Prelude.filter (`Map.notMember` condNames) (getConditions mob)
    when (not $ Prelude.null unk) do
      IO.hPutStr stderr fname
      IO.hPutStr stderr ": unknown conditions: "
      hPutStrLn stderr $ show unk

  case searchProto of
    Nothing -> pure ()
    Just sp -> when (sp == protoId (objInfo mob)) do
      IO.putStr fname
      IO.putStr " protoId = "
      print . protoId $ objInfo mob

getConditions :: Mob -> [Word32]
getConditions (Mob {..})
  = extract (Map.lookup (GeneralF Conditions) fields)
 <> extract (Map.lookup (GeneralF PermanentMods) fields)
  where
  f (CondArr cs) = pure cs
  f _ = Nothing

  extract = maybe [] content . (f =<<)

testConds :: [String] -> Mob -> [B.ByteString]
testConds cns = mapMaybe (`Map.lookup` needles) . getConditions
  where
  f name = (elfHash name, name)
  needles = Map.fromList $ f . B.pack <$> cns

checkUUID :: FilePath -> ObjectId -> Bool
checkUUID file vuuid = expectedUUIDString file == uuidStr
  where
  expectedUUIDString = dropExtensions . takeFileName

  uuidStr = objectIdToFileName vuuid

decodeOrFail :: (L.ByteString -> Either String a) -> Bool -> FilePath -> IO a
decodeOrFail decode quiet file =
  L.readFile file >>= \bs ->
    case decode bs of
      Left err -> do
        when (not quiet) do
          IO.hPutStr stderr file
          IO.hPutStr stderr ": "
          hPutStrLn stderr err
        exitWith $ ExitFailure 1
      Right result -> pure result

decodeMobOrFail :: Bool -> FilePath -> IO Mob
decodeMobOrFail = decodeOrFail decodeMob

decodeMobsOrFail :: FilePath -> IO [Mob]
decodeMobsOrFail = decodeOrFail decodeMobs False

loadMobsFromDirectory :: FilePath -> IO (Map.Map UUID Mob)
loadMobsFromDirectory loc = do
  files <- filter ext <$> listDirectory loc
  mobs <- traverse (decodeMobOrFail False . (loc </>)) files
  pure . Map.fromList $ withId <$> mobs
  where
  ext fn = ".mob" == fmap toLower (takeExtension fn)
  withId mob = (uuid $ objId mob, mob)

decodeDiffsOrFail :: Map.Map UUID Mob -> FilePath -> IO [(ObjectId, MobDiff)]
decodeDiffsOrFail mobs = decodeOrFail (decodeDiffs mobs) False

parseJsonOrFail :: FilePath -> IO Mob
parseJsonOrFail file = decodeOrFail (readMob file) False file

decodePlayerOrFail :: FilePath -> IO Player
decodePlayerOrFail = decodeOrFail decodePlayer False

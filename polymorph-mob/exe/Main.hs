module Main (main) where

import Control.Monad (when)
import Data.ByteString.Lazy as L
import Data.ByteString.Builder qualified as Bu
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.UUID (toString)
import Data.Word
import System.IO as IO
import System.Exit
import System.FilePath
import Text.Read (readMaybe)

import Options.Applicative

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
  | Json2Mob
    { _mmobOut :: Maybe FilePath
    , _jsonIn  :: FilePath
    }

data AnalyzeOpts
  = AO
  { quietFailure  :: Bool
  , reportSuccess :: Bool
  , searchProto   :: Maybe Word32
  }

inputArg :: Parser FilePath
inputArg = strArgument $ metavar "INPUT_FILE"

outputArg :: Parser FilePath
outputArg = strArgument $ metavar "OUPUT_FILE"

outputOpt :: Parser (Maybe FilePath)
outputOpt = option (maybeReader $ Just . Just)
          $ long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "Output file"
         <> value Nothing

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

analyzeOpts :: Parser AnalyzeOpts
analyzeOpts = AO <$> failure <*> success <*> proto where
  failure = switch $ long "quiet-failure" <> short 'F'
  success = switch $ long "report-success" <> short 's'
  proto = option (maybeReader $ fmap Just . readMaybe)
        $ long "proto"
       <> metavar "PROTO_ID"
       <> help "Print if prototype id matches"
       <> value Nothing

analyzeMob :: Mod CommandFields Action
analyzeMob = command "analyze-mob" $ info cmd desc where
  cmd = AnalyzeMob <$> analyzeOpts <*> inputArg
  desc = progDesc "Try to decode a MOB file and check information about it"

acts :: ParserInfo Action
acts = info (subs <**> helper) desc where
  subs = hsubparser $ mob2json <> mob2mob <> json2mob <> analyzeMob
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
  where
  p = prefs showHelpOnEmpty

performAnalysis :: FilePath -> AnalyzeOpts -> Mob -> IO ()
performAnalysis fname (AO {..}) mob = do
  when (not . checkUUID fname $ objId mob) do
    when (not quietFailure) do
      IO.hPutStr stderr fname
      hPutStrLn stderr ": UUID mismatch"
    exitWith $ ExitFailure 2

  when reportSuccess do
    IO.putStr fname
    putStrLn " OK"

  case searchProto of
    Nothing -> pure ()
    Just sp -> when (sp == protoId (objInfo mob)) do
      IO.putStr fname
      IO.putStr " protoId = "
      print . protoId $ objInfo mob

checkUUID :: FilePath -> ObjectId -> Bool
checkUUID file vuuid = expectedUUIDString file == uuidStr
  where
  expectedUUIDString = dropExtensions . takeFileName

  uuidStr = ("G_" ++) . tweak . toString $ uuid vuuid

  tweak [] = []
  tweak ('-':cs) = '_' : tweak cs
  tweak (c:cs) = toUpper c : tweak cs

decodeMobOrFail :: Bool -> FilePath -> IO Mob
decodeMobOrFail quietFailure file =
  L.readFile file >>= \bs ->
    case decodeMob bs of
      Left err -> do
        when (not quietFailure) do
          IO.hPutStr stderr file
          IO.hPutStr stderr ": "
          hPutStrLn stderr err
        exitWith $ ExitFailure 1
      Right mob -> pure mob

parseJsonOrFail :: FilePath -> IO Mob
parseJsonOrFail file =
  L.readFile file >>= \bs ->
    case readMob file bs of
      Left err -> do
        IO.hPutStr stderr file
        IO.hPutStr stderr ": "
        hPutStrLn stderr err
        exitWith $ ExitFailure 1
      Right mob -> pure mob

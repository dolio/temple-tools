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

import Decode
import Display
import Mob

data Action
  = Mob2Json
    { _jsonOut :: Maybe FilePath
    , _mobIn   :: FilePath
    }
  | AnalyzeMob
    { aopts :: AnalyzeOpts
    , mobIn :: FilePath
    }

data AnalyzeOpts
  = AO
  { quietFailure  :: Bool
  , reportSuccess :: Bool
  , searchProto   :: Maybe Word32
  }

inputArg :: Parser FilePath
inputArg = strArgument $ metavar "INPUT_FILE"

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
  subs = hsubparser $ mob2json <> analyzeMob
  desc = progDesc "manipulate various ToEE MOB representations"

main :: IO ()
main = customExecParser p acts >>= \case
  Mob2Json mout mobFile ->
    decodeMobOrFail False mobFile >>= \mob -> do
      Bu.writeFile out $ displayMob mob
      exitWith ExitSuccess
    where
    out = fromMaybe (mobFile <.> "json") mout
  AnalyzeMob {..} -> do
    decodeMobOrFail (quietFailure aopts) mobIn >>=
      performAnalysis mobIn aopts
    exitWith ExitSuccess
  where
  p = prefs showHelpOnEmpty

performAnalysis :: FilePath -> AnalyzeOpts -> Mob -> IO ()
performAnalysis fname (AO {..}) mob = do
  when (not . checkUUID fname $ vuuid mob) do
    when (not quietFailure) do
      IO.hPutStr stderr fname
      hPutStrLn stderr ": UUID mismatch"
    exitWith $ ExitFailure 2

  when reportSuccess do
    IO.putStr fname
    putStrLn " OK"

  case searchProto of
    Nothing -> pure ()
    Just sp -> when (sp == protoId mob) do
      IO.putStr fname
      IO.putStr " protoId = "
      print $ protoId mob

checkUUID :: FilePath -> VUUID -> Bool
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

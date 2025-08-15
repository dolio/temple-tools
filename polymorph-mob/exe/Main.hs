module Main (main) where

import Control.Monad (when)
import Data.ByteString.Lazy as L
import Data.ByteString.Builder qualified as Bu
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.UUID (UUID, toString)
import System.IO as IO
import System.Exit
import System.FilePath

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
    { _mobIn   :: FilePath
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

analyzeMob :: Mod CommandFields Action
analyzeMob = command "analyze-mob" $ info (AnalyzeMob <$> inputArg) desc where
  desc = progDesc "See if we can successfully decode a MOB file"

acts :: ParserInfo Action
acts = info (subs <**> helper) desc where
  subs = hsubparser $ mob2json <> analyzeMob
  desc = progDesc "manipulate various ToEE MOB representations"

main :: IO ()
main = customExecParser p acts >>= \case
  Mob2Json mout mobFile ->
    decodeMobOrFail mobFile >>= \mob -> do
      Bu.writeFile out $ displayMob mob
      exitWith ExitSuccess
    where
    out = fromMaybe (mobFile <.> "json") mout
  AnalyzeMob mobFile -> do
    mob <- decodeMobOrFail mobFile
    when (not $ checkUUID mobFile (uuid mob)) do
      IO.hPutStr stderr mobFile
      hPutStrLn stderr ": UUID mismatch"
      exitWith $ ExitFailure 2
    exitWith ExitSuccess
  where
  p = prefs showHelpOnEmpty


checkUUID :: FilePath -> UUID -> Bool
checkUUID file uuid = expectedUUIDString file == uuidStr
  where
  expectedUUIDString = dropExtensions . takeFileName

  uuidStr = ("G_" ++) . tweak $ toString uuid

  tweak [] = []
  tweak ('-':cs) = '_' : tweak cs
  tweak (c:cs) = toUpper c : tweak cs

decodeMobOrFail :: FilePath -> IO Mob
decodeMobOrFail file =
  L.readFile file >>= \bs ->
    case decodeMob bs of
      Left err -> do
        IO.hPutStr stderr file
        IO.hPutStr stderr ": "
        hPutStrLn stderr err
        exitWith $ ExitFailure 1
      Right mob -> pure mob

module Main (main) where

import Data.ByteString.Lazy as L
import Data.ByteString.Builder qualified as Bu
import Data.Maybe (fromMaybe)
import System.FilePath

import Options.Applicative


import Decode
import Display

data Action
  = Mob2Json
  { _jsonOut :: Maybe FilePath
  , _mobIn   :: FilePath
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

mob2json :: ParserInfo Action
mob2json = info (acts <**> helper) desc where
  acts = hsubparser $ command "mob-to-json" m2j

  m2j = info (Mob2Json <$> outputOpt <*> inputArg) m2jdesc
  m2jdesc = progDesc "Turn a MOB file into (somewhat) readable JSON"

  desc = progDesc "convert between various ToEE MOB representations"

main :: IO ()
main = customExecParser p mob2json >>= \case
  Mob2Json mout mobFile -> do
    mob <- decodeMob <$> L.readFile mobFile
    Bu.writeFile out (displayMob mob)
    where
    out = fromMaybe (mobFile <.> "json") mout
  where
  p = prefs showHelpOnEmpty

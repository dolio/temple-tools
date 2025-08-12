module Main (main) where

import Control.Monad (guard)
import Data.ByteString (ByteString, readFile)
import Data.ByteString.Builder (Builder, writeFile)
import Options.Applicative
import System.Exit
import System.IO hiding (readFile, writeFile)

import Prelude hiding (readFile, writeFile)

import Temple.Tables.Help as Help

data Format
  = Help
  deriving (Read)

data Mode
  = TestTab
  | TestText
  | Forward
  | Reverse
  deriving (Eq, Ord, Show)

formatOpt :: Parser Format
formatOpt = option reader
          $ long "format"
         <> short 'f'
         <> metavar "FORMAT"
         <> help "Table format"
         <> value Help
  where
  reader = maybeReader $ \s -> Help <$ guard (s == "help")

inputOpt :: Parser FilePath
inputOpt = strOption
         $ long "input"
        <> short 'i'
        <> metavar "FILE"
        <> help "Input file"

outputOpt :: Parser FilePath
outputOpt = strOption
          $ long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "Ouput file"

modeOpt :: Parser Mode
modeOpt = asum
  [ flag' Reverse
    $ long "reverse"
   <> help "rebuild a tab file from text files"
  , flag' TestTab
    $ long "test-tab"
   <> help "test tab file idempotence"
  , flag' TestText
    $ long "test-text"
   <> help "test text file idempotence"
  , pure Forward
  ]

strictSwitch :: Parser Bool
strictSwitch = switch
             $ long "strict"
            <> help "don't ignore malformed entries"

args :: Parser (Bool, Mode, Format, FilePath, FilePath)
args =
  (,,,,) <$> strictSwitch <*> modeOpt <*> formatOpt <*> inputOpt <*> outputOpt

data Codec where
  Codec :: (FilePath -> ByteString -> Either String e)
        -> (e -> Builder)
        -> Codec

main :: IO ()
main = execParser (info (args <**> helper) desc) >>= \case
  (strict, mo, fmt, i, o)
    | Codec dec enc <- resolveCodec strict fmt mo -> do
      readFile i >>= \input -> case dec i input of
        Left msg -> do
          hPutStrLn stderr "The spell fizzles"
          hPutStrLn stderr ""
          hPutStrLn stderr msg
          exitWith $ ExitFailure 1
        Right content -> writeFile o $ enc content
  where
  desc = fullDesc
      <> progDesc myDesc
      <> header "Transmute Tab to Text"

  readBinaryFile i = openBinaryFile i ReadMode >>= hGetContents

myDesc =
  "Tool for translating between ToEE table files and more readable formats"

resolveCodec :: Bool -> Format -> Mode -> Codec
resolveCodec strict Help TestTab
  = Codec (readHelpTable strict) writeHelpTable
resolveCodec strict Help TestText
  = Codec (readEntryFile strict) prettyEntries
resolveCodec strict Help Forward
  = Codec (readHelpTable strict) prettyEntries
resolveCodec strict Help Reverse
  = Codec (readEntryFile strict) writeHelpTable

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile file str = withBinaryFile file WriteMode (flip hPutStr str)

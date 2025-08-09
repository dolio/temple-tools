module Main (main) where

import Control.Exception
import Options.Applicative
import System.IO

import Footer
import Entry
import Tree

data Opts = O { _verbose :: Bool, _directory :: FilePath }

data Action = List FilePath | Extract FilePath

dirOpt :: Parser FilePath
dirOpt = strOption
       $ long "dir"
      <> short 'd'
      <> metavar "TARGET"
      <> help "Target directory"
      <> value "."
      <> showDefault

fileArg :: Parser FilePath
fileArg = strArgument $ metavar "FILE"

verb :: Parser Bool
verb = switch
     $ long "verbose"
    <> short 'v'
    <> help "Enable verbose mode"

extract :: ParserInfo Action
extract = info ex desc
  where
  ex = Extract <$> fileArg
  desc = progDesc "Extract the contents of a Troika DAT file"

list :: ParserInfo Action
list = info li desc
  where
  li = List <$> fileArg
  desc = progDesc "List the contents of a Troika DAT file"

opts :: Parser Opts
opts = O <$> verb <*> dirOpt

act :: ParserInfo (Opts, Action)
act = info ((,) <$> opts <*> cmd <**> helper) desc
  where
  cmd = hsubparser $ command "list" list <> command "extract" extract
  desc = fullDesc
      <> progDesc "Extract or list the contents of a Troika DAT file"
      <> header "Discern DAT"

main :: IO ()
main = customExecParser p act >>= \case
  (O v dir, List file) -> do
    (h, tree) <- prime file
    hClose h
    putStrLn $ displayDirectoryTree dir tree
  (O v dir, Extract file) -> do
    (h, tree) <- prime file
    extractFromHandle h dir tree
    hClose h
  where
  p = prefs $ showHelpOnEmpty <> subparserInline

-- Common setup for both commands, opens a file and constructs the embedded
-- directory tree.
prime :: FilePath -> IO (Handle, DirectoryTree)
prime file = do
  h <- openFile file ReadMode
  hSeek h SeekFromEnd (-12)
  foot <- getFooter h
  hSeek h SeekFromEnd . negate . fromIntegral $ tableOffset foot
  entries <- getEntries h . fromIntegral $ tableOffset foot
  tree <- evaluate $ buildDirectoryTree entries
  pure (h, tree)


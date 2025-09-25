module Main (main) where

import Control.Exception
import Control.Monad
import Data.Maybe (fromMaybe)
import Options.Applicative
import System.FilePath
import System.IO

import Temple.Dat.Footer
import Temple.Dat.Entry
import Temple.Dat.Tree

data Action
  = List { _verbose :: Bool, _out :: Maybe FilePath, _in :: FilePath }
  | Disjoin { _verbose :: Bool, _out :: Maybe FilePath, _in :: FilePath }
  | Fabricate { _verbose :: Bool, _out :: Maybe FilePath, _in :: FilePath }

dirOpt :: Parser (Maybe FilePath)
dirOpt = option (maybeReader $ Just . Just)
       $ long "dir"
      <> short 'd'
      <> metavar "TARGET"
      <> help "Target directory"
      <> value Nothing
      <> action "directory"

outOpt :: Parser (Maybe FilePath)
outOpt = option (maybeReader $ Just . Just)
       $ long "output"
      <> short 'o'
      <> metavar "FILE"
      <> help "Output file"
      <> value Nothing
      <> action "file"

fileArg :: Parser FilePath
fileArg = strArgument
        $ metavar "FILE"
       <> action "file"

dirArg :: Parser FilePath
dirArg = strArgument
       $ metavar "DIRECTORY"
      <> action "directory"

verb :: Parser Bool
verb = switch
     $ long "verbose"
    <> short 'v'
    <> help "Enable verbose mode"

disjoin :: ParserInfo Action
disjoin = info ex desc
  where
  ex = Disjoin <$> verb <*> dirOpt <*> fileArg
  desc = progDesc "Extract the contents of a Troika DAT file"

list :: ParserInfo Action
list = info li desc
  where
  li = List <$> verb <*> dirOpt <*> fileArg
  desc = progDesc "List the contents of a Troika DAT file"

fabricate :: ParserInfo Action
fabricate = info fa desc
  where
  fa = Fabricate <$> verb <*> outOpt <*> dirArg
  desc = progDesc "Create a Troika DAT file from a directory"

act :: ParserInfo Action
act = info (cmd <**> helper) desc
  where
  cmd = hsubparser
      $ command "list" list
     <> command "disjoin" disjoin
     <> command "fabricate" fabricate
  desc = fullDesc
      <> progDesc "Extract or list the contents of a Troika DAT file"
      <> header "Craft (DAT)"

main :: IO ()
main = customExecParser p act >>= \case
  List v (fromMaybe "." -> dir) file -> do
    (h, tree) <- prime v file
    hClose h
    putStrLn $ displayDirectoryTree dir tree
  Disjoin v (fromMaybe "." -> dir) file -> do
    (h, tree) <- prime v file
    when v $ hPutStr stderr "Extracting files\n"
    extractFromHandle h dir tree
    hClose h
  Fabricate _v mout dir -> withFile out WriteMode \h -> do
    dt <- buildFromDirectory dir
    et <- compressAndNumber h dt
    preLoc <- hTell h
    writeEntries h $ snd <$> flattenTree et
    postLoc <- hTell h
    writeFooter h =<< createFooter (fromIntegral $ postLoc - preLoc + 28)
    where
    out = fromMaybe (dir <.> "dat") mout
  where
  p = prefs $ showHelpOnEmpty <> subparserInline

-- Common setup for both dat-input commands, opens a file and constructs the
-- embedded directory tree.
prime :: Bool -> FilePath -> IO (Handle, DirectoryTree () FileInfo)
prime verbose file = do
  h <- openFile file ReadMode
  hSeek h SeekFromEnd (-12)
  foot <- readFooter h
  hSeek h SeekFromEnd . negate . fromIntegral $ tableOffset foot
  when verbose $ hPutStr stderr "Getting entries\n"
  entries <- getEntries h . fromIntegral $ tableOffset foot
  when verbose $ hPutStr stderr "Building directory tree\n"
  tree <- evaluate $ buildDirectoryTree entries
  pure (h, tree)


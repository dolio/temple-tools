module Main (main) where

import Control.Exception
import Control.Monad
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.ByteString.Builder qualified as BU
import Data.UUID
import Options.Applicative
import System.FilePath
import System.IO
import Text.Read

import Temple.Dat.Footer
import Temple.Dat.Entry
import Temple.Dat.Tree

data Action
  = Discern
  { _verbose :: Bool
  , _check :: Bool
  , _out :: Maybe FilePath
  , _in :: FilePath
  }
  | Disjoin
  { _verbose :: Bool
  , _out :: Maybe FilePath
  , _in :: FilePath
  }
  | Fabricate
  { _verbose :: Bool
  , _guid :: Maybe UUID
  , _out :: Maybe FilePath
  , _in :: FilePath
  }
  | Unite
  { _guid :: Maybe UUID
  , _comb :: FilePath
  , _ins :: [FilePath]
  }

guidOpt :: Parser (Maybe UUID)
guidOpt = option (maybeReader $ fmap Just . readMaybe)
        $ long "guid"
       <> short 'G'
       <> metavar "GUID"
       <> value Nothing

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

outOpt' :: Parser FilePath
outOpt' = strOption
        $ long "output"
       <> short 'o'
       <> metavar "FILE"
       <> help "Output file"
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

check :: Parser Bool
check = switch
      $ long "check"
     <> short 'C'
     <> help "Validate checksums of a craft-dat created file"

disjoin :: ParserInfo Action
disjoin = info ex desc
  where
  ex = Disjoin <$> verb <*> dirOpt <*> fileArg
  desc = progDesc "Extract the contents of a Troika DAT file"

discern :: ParserInfo Action
discern = info li desc
  where
  li = Discern <$> verb <*> check <*> dirOpt <*> fileArg
  desc = progDesc "List/check the contents of a Troika DAT file"

fabricate :: ParserInfo Action
fabricate = info fa desc
  where
  fa = Fabricate <$> verb <*> guidOpt <*> outOpt <*> dirArg
  desc = progDesc "Create a Troika DAT file from a directory"

unite :: ParserInfo Action
unite = info un desc
  where
  some2 p = (:) <$> p <*> some p
  un = Unite <$> guidOpt <*> outOpt' <*> some2 fileArg
  desc = progDesc "Combine multiple Troika DAT files (later take precedence)"

act :: ParserInfo Action
act = info (cmd <**> helper) desc
  where
  cmd = hsubparser
      $ command "discern" discern
     <> command "disjoin" disjoin
     <> command "fabricate" fabricate
     <> command "unite" unite
  desc = fullDesc
      <> progDesc "Manipulate Troika DAT files"
      <> header "Craft (DAT)"

main :: IO ()
main = customExecParser p act >>= \case
  Discern v ck (fromMaybe "." -> dir) file ->
    prime v file \h version tree -> do
      for_ version \uuid ->
        putStr "DAT id: " *> print uuid *> putStrLn ""
      displayDirectoryTree (guard ck *> pure h) (BU.string8 dir) tree
  Disjoin v (fromMaybe "." -> dir) file ->
    prime v file \h version tree -> do
      when v $ for_ version \uuid ->
        putStr "DAT id: " *> print uuid *> putStrLn ""
      when v $ hPutStrLn stderr "Extracting files"
      extractFromHandle h dir tree
  Fabricate _v mguid mout dir ->
    withFile out WriteMode \h ->
      writeEntryTree h mguid
        =<< compressAndNumber h
        =<< buildFromDirectory dir
    where
    out = fromMaybe (dropTrailingPathSeparator dir <.> "dat") mout
  Unite mguid out ins ->
    primes ins \his dts ->
    withFile out WriteMode \ho ->
      writeEntryTree ho mguid
        =<< coalesceAndNumber his ho dts
  where
  p = prefs $ showHelpOnEmpty <> subparserInline

-- Common setup for both dat-input commands, opens a file and constructs the
-- embedded directory tree.
prime :: Bool
      -> FilePath
      -> (Handle -> Maybe UUID -> BasicTree -> IO r)
      -> IO r
prime verbose file k = withFile file ReadMode \h ->
  initializeDat verbose h >>= uncurry (k h)

primes :: [FilePath] -> ([Handle] -> DirectoryTrees -> IO r) -> IO r
primes fs k = opens [] mempty fs where
  opens hs ds [] = k (reverse hs) ds
  opens hs ds (f:fs) =
    prime False f \h _ t ->
      opens (h:hs) (ds <> singleSource t) fs

initializeDat :: Bool -> Handle -> IO (Maybe UUID, BasicTree)
initializeDat verbose h = do
  hSeek h SeekFromEnd (-12)
  foot <- readFooter h
  hSeek h SeekFromEnd . negate . fromIntegral $ tableOffset foot
  when verbose $ hPutStrLn stderr "Getting entries"
  entries <- getEntries h . fromIntegral $ tableOffset foot
  when verbose $ hPutStrLn stderr "Building directory tree"
  tree <- evaluate $ buildDirectoryTree entries
  pure (version foot, tree)

writeEntryTree :: Handle -> Maybe UUID -> EntryTree -> IO ()
writeEntryTree h mguid et = do
  compSz <- fromIntegral <$> hTell h
  let preLoc = compSz + 4
  BU.hPutBuilder h $ BU.word32LE preLoc
  namesSize <- writeEntries h $ snd <$> flattenTree et
  postLoc <- fromIntegral <$> hTell h
  let tableOff = postLoc - preLoc + 28
  writeFooter h =<< footer tableOff namesSize mguid
  where
  footer off size = \case
    Nothing -> createFooter off size
    Just guid -> pure $ FO off size (Just guid)

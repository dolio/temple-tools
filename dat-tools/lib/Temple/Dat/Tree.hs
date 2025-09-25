module Temple.Dat.Tree
  ( FileInfo (..)
  , DirectoryTree (..)
  , buildDirectoryTree
  , displayDirectoryTree
  , extractFromHandle
  , buildFromDirectory
  , compressAndNumber
  , flattenTree
  ) where

import Codec.Compression.Zlib
import Control.Monad.State
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as L
import Data.HashMap.Strict as HM
import Data.HashSet qualified as HS
import Data.List (mapAccumR)
import Data.Traversable (for)
import Data.Word
import System.Directory
import System.FilePath ((</>))
import System.IO

import Temple.Dat.Entry (Entry)
import Temple.Dat.Entry qualified as E

-- Information for an archived file.
data FileInfo
  = FI
  { compressed :: Bool -- whether the archived bytes are compressed
  , fullSize :: Word32 -- original file size
  , packSize :: Word32 -- size in the archvie
  , offset   :: Word32 -- starting location of the archived bytes
  }

-- A path in a directory tree is a sequence of directory names
type Path = [ByteString]

-- A directory tree has files that are necessarily leaves, and directories
-- that allow other trees to be nested below them. The name information for
-- files is stored implicitly along the branching structure.
--
-- The parameters are additional information for the directory and file nodes
-- of the tree.
data DirectoryTree dir file
  = File file
  | Branch dir (HashMap ByteString (DirectoryTree dir file))
  deriving (Functor)

instance Bifunctor DirectoryTree where
  bimap _ g (File x) = File (g x)
  bimap f g (Branch x subs) = Branch (f x) (HM.map (bimap f g) subs)

instance Bifoldable DirectoryTree where
  bifoldMap _ g (File x) = g x
  bifoldMap f g (Branch x subs) = f x <> foldMap (bifoldMap f g) subs

instance Bitraversable DirectoryTree where
  bitraverse _ g (File x) = File <$> g x
  bitraverse f g (Branch x subs) =
    Branch <$> f x <*> traverse (bitraverse f g) subs

emptyDirs :: DirectoryTree () file
emptyDirs = Branch () empty

-- Essentially the analogue of `mkdir -p`, creates a multi-level directory
-- tree for the specified non-empty path.
build0 :: Path -> ByteString -> DirectoryTree () file -> DirectoryTree () file
build0 [    ] nm = Branch () . HM.singleton nm
build0 (p:ps) nm = Branch () . HM.singleton p . build0 ps nm

-- Inserts a child tree at the specified non-empty path in a parent tree.
-- Overwrites anything that is already at that position.
insert0
  :: Path
  -> ByteString
  -> DirectoryTree () file -- child tree
  -> DirectoryTree () file -- parent tree
  -> DirectoryTree () file
insert0  _     nm _ (File _) = error msg
  where
  msg = "bad DAT: nested under file: " ++ "/" ++ C8.unpack nm
insert0 [    ] nm v (Branch u chl) = Branch u $ HM.insert nm v chl
insert0 (p:ps) nm v (Branch u chl) = Branch u $ HM.alter f p chl
  where
  f Nothing = Just $ build0 ps nm v
  f (Just tr) = Just $ insert0 ps nm v tr

-- Creates a tree node from an entry. Directories are just empty branches,
-- while files copy some information.
treeFromEntry :: Entry -> DirectoryTree () FileInfo
treeFromEntry e
  | E.isDirectory e = emptyDirs
  | otherwise =
    File $ FI (E.isCompressed e) (E.fullSize e) (E.packSize e) (E.offset e)

-- Main worker for building a `DirectoryTree` from a list of numbered
-- `Entry` values.
--
-- ps :: HashMap Word32 Path
--   a mapping from entry numbers to their full path in the directory tree
--
-- acc :: DirectoryTree
--   the directory tree we are building up
--
-- os :: [(Word32, Entry)]
--   Entries we weren't able to insert yet, because their parents weren't
--   in the directory tree. Seems like most archives are not designed to
--   required this.
--
-- es :: [(Word32, Entry)]
--   The list of entries yet to be processed.
--
-- Once we consume all of `es`, we restart with `os` and hope we've
-- inserted enough to process more of them. However, if there is a bad
-- directory structure with entries whose parents don't exist, this will
-- simply loop forever.
--
-- Also note: the root level is -1, so `ps` should be bootstrapped with
-- an initial value for that.
consume
  :: HashMap Word32 Path
  -> DirectoryTree () FileInfo
  -> [(Word32, Entry)]
  -> [(Word32, Entry)]
  -> DirectoryTree () FileInfo
consume  _ acc [] [           ] = acc
consume ps acc os [           ] = consume ps acc [] $ reverse os
consume ps acc os (ne@(n,e):es)
  -- Refuse to work with a DAT if the file names look weird, like they're
  -- absolute paths or something.
  | C8.any (`HS.member` badChars) $ E.name e =
      error $ "bad file name in dat: " ++ C8.unpack (E.name e)
  | otherwise = case HM.lookup (E.parent e) ps of
  Just path
    | ps <- insert n (path ++ [E.name e]) ps ->
    consume ps (insert0 path (E.name e) (treeFromEntry e) acc) os es
  Nothing ->
    consume ps acc (ne:os) es
  where
  badChars = HS.fromList "\\/:"

-- Creates a nested directory tree from a list of entry information.
buildDirectoryTree :: [Entry] -> DirectoryTree () FileInfo
buildDirectoryTree = consume root emptyDirs [] . zip [0..]
  where root = HM.singleton (-1) []

-- Displays a directory structure as a complete listing
displayDirectoryTree :: String -> DirectoryTree () FileInfo -> String
displayDirectoryTree root d0 = descend (showString root) d0 ""
  where
  descend :: ShowS -> DirectoryTree () FileInfo -> ShowS
  descend path (File f) =
    path . showString (if compressed f then " (compressed)\n" else "\n")
  descend path (Branch _ ds) =
    path . showString "/\n" . foldMapWithKey f ds
    where
    f p dt = descend (path . showString "/" . showString (C8.unpack p)) dt

getFileData :: Handle -> FileInfo -> IO L.ByteString
getFileData h f = do
  hSeek h AbsoluteSeek . fromIntegral $ offset f
  inflate <$> L.hGet h (fromIntegral $ packSize f)
  where
  inflate | compressed f = decompress
          | otherwise = id

extractFromHandle :: Handle -> FilePath -> DirectoryTree () FileInfo -> IO ()
extractFromHandle h = descend where
  descend name (Branch _ ds) = do
    createDirectoryIfMissing True name
    withCurrentDirectory name $ foldMapWithKey (descend . C8.unpack) ds
  descend name (File f) = getFileData h f >>= L.writeFile name

-- Builds a directory tree representing the directories below a specified
-- directory.
buildFromDirectory :: FilePath -> IO (DirectoryTree () FilePath)
buildFromDirectory root =
  doesDirectoryExist root >>= \case
    True -> do
      subs <- listDirectory root
      assocs <- for subs \sub ->
        (C8.pack sub,) <$> buildFromDirectory (root </> sub)
      pure . Branch () $ HM.fromList assocs
    False -> doesFileExist root >>= \case
      True -> pure $ File root
      False -> error "buildFromDirectory: bad file argument"

-- Compresses the files in a `DirectoryTree` to a handle, while numbering all
-- the entries in the tree. The file paths at the leaves are replaced by
-- `FileInfo` values containing the relevant information about the compressed
-- bytes written to the handle.
compressAndNumber
  :: Handle
  -> DirectoryTree () FilePath
  -> IO (DirectoryTree Word32 (Word32, FileInfo))
compressAndNumber h dt = evalStateT (bitraverse d f dt) (-1) where
  d _ = state \n -> (n, n+1)

  f path = StateT \n -> do
    bs <- L.readFile path
    let fsz = fromIntegral $ L.length bs
        cs = compress bs
        psz = fromIntegral $ L.length cs
    off <- fromIntegral <$> hTell h
    L.hPut h cs
    pure ((n, FI True fsz psz off), n+1)

rootNumber :: DirectoryTree n (n, file) -> n
rootNumber (File (n, _)) = n
rootNumber (Branch n _) = n

type EntryTree = DirectoryTree Word32 (Word32, FileInfo)

swizzle
  :: HM.HashMap ByteString EntryTree
  -> (Word32, [(ByteString, Word32, EntryTree)])
swizzle = mapAccumR thread (-1) . HM.toList
  where thread next (name, dt) = (rootNumber dt, (name, next, dt))

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

flattenTree :: DirectoryTree Word32 (Word32, FileInfo) -> [(Word32, Entry)]
flattenTree (File _) = error "flattenTree: file root"
flattenTree (Branch n (swizzle -> (_, bs))) = flats n bs
  where
  flats n bs = uncurry3 (flat n) =<< bs

  flat parent name next = \case
    File (n, FI {..}) ->
      [(n, E.EN name attrs fullSize packSize offset parent 0 next)]
      where
      attrs = if compressed then 0x2 else 0
    Branch n (swizzle -> (first, bs)) ->
      (n, E.EN name attrs 0 0 0 parent first next) : flats n bs
      where
      attrs = 0x400 -- directory


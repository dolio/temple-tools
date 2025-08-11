module Tree
  ( FileInfo (..)
  , DirectoryTree (..)
  , buildDirectoryTree
  , displayDirectoryTree
  , extractFromHandle
  ) where

import Codec.Compression.Zlib
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Char8 qualified as C8
import Data.HashMap.Strict as HM
import Data.HashSet qualified as HS
import Data.Word
import System.Directory
import System.IO

import Entry (Entry)
import Entry qualified as E

-- Information for an archived file.
data FileInfo
  = FI
  { compressed :: Bool -- whether the archived bytes are compressed
  , fullSize :: Word32 -- original file size
  , packSize :: Word32 -- size in the archvie
  , offset :: Word32   -- starting location of the archived bytes
  }

-- A path in a directory tree is a sequence of directory names
type Path = [ByteString]

-- A directory tree has files that are necessarily leaves, and directories
-- that allow other trees to be nested below them. The name information for
-- files is stored implicitly along the branching structure.
data DirectoryTree
  = File FileInfo
  | Branch (HashMap ByteString DirectoryTree)

emptyDirs :: DirectoryTree
emptyDirs = Branch empty

-- Essentially the analogue of `mkdir -p`, creates a multi-level directory
-- tree for the specified non-empty path.
build0 :: Path -> ByteString -> DirectoryTree -> DirectoryTree
build0 [    ] nm = Branch . HM.singleton nm
build0 (p:ps) nm = Branch . HM.singleton p . build0 ps nm

-- Inserts a child tree at the specified non-empty path in a parent tree.
-- Overwrites anything that is already at that position.
insert0
  :: Path
  -> ByteString
  -> DirectoryTree -- child tree
  -> DirectoryTree -- parent tree
  -> DirectoryTree
insert0  _     nm _ (File _) = error msg
  where
  msg = "bad DAT: nested under file: " ++ "/" ++ C8.unpack nm
insert0 [    ] nm v (Branch chl) = Branch $ HM.insert nm v chl
insert0 (p:ps) nm v (Branch chl) = Branch $ HM.alter f p chl
  where
  f Nothing = Just $ build0 ps nm v
  f (Just tr) = Just $ insert0 ps nm v tr

-- Creates a tree node from an entry. Directories are just empty branches,
-- while files copy some information.
treeFromEntry :: Entry -> DirectoryTree
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
  -> DirectoryTree
  -> [(Word32, Entry)]
  -> [(Word32, Entry)]
  -> DirectoryTree
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
buildDirectoryTree :: [Entry] -> DirectoryTree
buildDirectoryTree = consume root emptyDirs [] . zip [0..]
  where root = HM.singleton (-1) []

-- Displays a directory structure as a complete listing
displayDirectoryTree :: String -> DirectoryTree -> String
displayDirectoryTree root d0 = descend (showString root) d0 ""
  where
  descend :: ShowS -> DirectoryTree -> ShowS
  descend path (File    f) =
    path . showString (if compressed f then " (compressed)\n" else "\n")
  descend path (Branch ds) =
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

extractFromHandle :: Handle -> FilePath -> DirectoryTree -> IO ()
extractFromHandle h = descend where
  descend name (Branch ds) = do
    createDirectoryIfMissing True name
    withCurrentDirectory name $ foldMapWithKey (descend . C8.unpack) ds
  descend name (File f) = getFileData h f >>= L.writeFile name


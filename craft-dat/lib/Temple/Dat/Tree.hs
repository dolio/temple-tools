module Temple.Dat.Tree
  ( FileInfo (..)
  , DirectoryTree (..)
  , BasicTree
  , EntryTree
  , buildDirectoryTree
  , displayDirectoryTree
  , extractFromHandle
  , buildFromDirectory
  , DirectoryTrees (..)
  , singleSource
  , compressAndNumber
  , coalesceAndNumber
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
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BU
import Data.Char (toLower)
import Data.Digest.CRC32
import Data.Map.Strict as M hiding ((!?))
import Data.List (mapAccumR, intersperse)
import Data.Primitive.Array (Array, arrayFromList, indexArray, sizeofArray)
import Data.Traversable (for)
import Data.Word
import System.Directory
import System.FilePath ((</>))
import System.IO

import Temple.Dat.Entry (Entry)
import Temple.Dat.Entry qualified as E

(!?) :: Array a -> Word32 -> Maybe a
a !? (fromIntegral -> i)
  | i < sizeofArray a = Just (indexArray a i)
  | otherwise = Nothing
{-# inline (!?) #-}

-- Information for an archived file.
data FileInfo
  = FI
  { compressed :: !Bool -- whether the archived bytes are compressed
  , misc     :: !Word32 -- extra data
  , fullSize :: !Word32 -- original file size
  , packSize :: !Word32 -- size in the archvie
  , offset   :: !Word32 -- starting location of the archived bytes
  }

data UncasedString = US { orig :: !ByteString, lower :: !ByteString }

packUncase :: String -> UncasedString
packUncase = uncase . C8.pack

uncase :: ByteString -> UncasedString
uncase orig = US {..}
  where lower = C8.map toLower orig

unpackOrig :: UncasedString -> String
unpackOrig = C8.unpack . orig

instance Eq UncasedString where
  l == r = lower l == lower r
  l /= r = lower l /= lower r

instance Ord UncasedString where
  compare l r = compare (lower l) (lower r)

entryToFileInfo :: Entry -> FileInfo
entryToFileInfo e =
  FI (E.isCompressed e) (E.misc e) (E.fullSize e) (E.packSize e) (E.offset e)

-- A directory tree has files that are necessarily leaves, and directories
-- that allow other trees to be nested below them. The name information for
-- files is stored implicitly along the branching structure.
--
-- The parameters are additional information for the directory and file nodes
-- of the tree.
data DirectoryTree dir file
  = File file
  | Branch dir (Map UncasedString (DirectoryTree dir file))
  deriving (Functor)

instance Bifunctor DirectoryTree where
  bimap _ g (File x) = File (g x)
  bimap f g (Branch x subs) = Branch (f x) (M.map (bimap f g) subs)

instance Bifoldable DirectoryTree where
  bifoldMap _ g (File x) = g x
  bifoldMap f g (Branch x subs) = f x <> foldMap (bifoldMap f g) subs

instance Bitraversable DirectoryTree where
  bitraverse _ g (File x) = File <$> g x
  bitraverse f g (Branch x subs) =
    Branch <$> f x <*> traverse (bitraverse f g) subs

emptyDirs :: DirectoryTree () file
emptyDirs = Branch () empty

type BasicTree = DirectoryTree () FileInfo

-- This represents a combined directory structure from multiple DATs. Each
-- node in the tree is annotated with which source it's from.
data DirectoryTrees
  = DTs
  { sources :: !Int
  , combined :: DirectoryTree () (Int, FileInfo)
  }

singleSource :: BasicTree -> DirectoryTrees
singleSource t = DTs 1 (fmap (0,) t)

instance Semigroup DirectoryTrees where
  DTs 0 _ <> r = r
  l <> DTs 0 _ = l
  DTs m tl <> DTs n tr = DTs (m+n) (combine tl $ fmap (first (+m)) tr)
    where
    combine (File _) (File r) = File r
    combine (Branch _ l) (Branch _ r) =
      Branch () (unionWith combine l r)
    combine _ _ = error "DirectoryTrees merge: File aligned with Directory"

instance Monoid DirectoryTrees where
  mempty = DTs 0 emptyDirs

crawlRight
  :: Array Entry
  -> Entry
  -> [(UncasedString, DirectoryTree () FileInfo)]
crawlRight es e = (key, tree) : case es !? E.nextSibling e of
  Nothing -> []
  Just e -> crawlRight es e
  where
  key = uncase $ E.name e
  tree | not $ E.isDirectory e = File $ entryToFileInfo e
       | Just c <- es !? E.firstChild e =
         Branch () . M.fromList $ crawlRight es c
       | otherwise = emptyDirs

-- Creates a nested directory tree from a list of entries. This uses the
-- information on child/sibling entries, and assumes they correspond to the
-- positions in the list.
buildDirectoryTree :: [Entry] -> DirectoryTree () FileInfo
buildDirectoryTree (arrayFromList -> !es) = case es !? 0 of
  Nothing -> emptyDirs
  Just e -> Branch () . M.fromList $ crawlRight es e

intercalateMap :: Builder -> (a -> Builder) -> [a] -> Builder
intercalateMap mid f = mconcat . intersperse mid . fmap f

-- Displays a directory structure as a complete listing. If a handle is
-- provided, it will be used to check CRCs of files presumed to be in the
-- 'misc' field.
displayDirectoryTree ::
  Maybe Handle -> Builder -> DirectoryTree () FileInfo -> IO ()
displayDirectoryTree mh root d0 = descend root d0
  where
  dispInfo [] = ""
  dispInfo is = " (" <> intercalateMap ", " BU.byteString is <> ")"

  compInfo fi = if compressed fi then ["compressed"] else []

  fileInfo
    | Just h <- mh = \fi -> do
      bs <- getFileData h fi
      let CRC32 cks = digest $ L.toStrict bs
      if cks == misc fi
      then pure $ compInfo fi ++ ["checksum ok"]
      else pure $ compInfo fi ++ ["checksum mismatch"]
    | otherwise = pure . compInfo

  descend :: Builder -> DirectoryTree () FileInfo -> IO ()
  descend path (File f) = do
    finf <- fileInfo f
    BU.hPutBuilder stdout $ path <> dispInfo finf
    putStrLn ""
  descend path (Branch _ ds) = do
    BU.hPutBuilder stdout path
    putStrLn "/"
    foldMapWithKey f ds
    where
    f p dt = descend (path <> "/" <> BU.byteString (orig p)) dt

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
    withCurrentDirectory name $ foldMapWithKey (descend . unpackOrig) ds
  descend name (File f) = do
    bs <- getFileData h f
    L.writeFile name bs

-- Builds a directory tree representing the directories below a specified
-- directory.
buildFromDirectory :: FilePath -> IO (DirectoryTree () FilePath)
buildFromDirectory root =
  doesDirectoryExist root >>= \case
    True -> do
      subs <- listDirectory root
      assocs <- for subs \sub ->
        (packUncase sub,) <$> buildFromDirectory (root </> sub)
      pure . Branch () $ M.fromList assocs
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
  -> IO EntryTree
compressAndNumber h dt = evalStateT (bitraverse d f dt) (-1) where
  d _ = state \n -> (n, n+1)

  f path = StateT \n -> do
    bs <- L.readFile path
    let fsz = fromIntegral $ L.length bs
        cparms = defaultCompressParams {compressLevel = bestCompression}
        cs = compressWith cparms bs
        -- only compress if it actually decreases the size
        compr = L.length cs < L.length bs
        psz = fromIntegral $ L.length cs
        CRC32 cks = digest $ L.toStrict bs
    off <- fromIntegral <$> hTell h
    L.hPut h (if compr then cs else bs)
    pure ((n, FI compr cks fsz (min fsz psz) off), n+1)

rootNumber :: DirectoryTree n (n, file) -> n
rootNumber (File (n, _)) = n
rootNumber (Branch n _) = n

type EntryTree = DirectoryTree Word32 (Word32, FileInfo)

swizzle
  :: Map UncasedString EntryTree
  -> (Word32, [(ByteString, Word32, EntryTree)])
swizzle = mapAccumR thread (-1) . M.toList
  where thread next (name, dt) = (rootNumber dt, (orig name, next, dt))

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

flattenTree :: EntryTree -> [(Word32, Entry)]
flattenTree (File _) = error "flattenTree: file root"
flattenTree (Branch n (swizzle -> (_, bs))) = flats n bs
  where
  flats n bs = uncurry3 (flat n) =<< bs

  flat parent name next = \case
    File (n, FI {..}) ->
      [(n, E.EN name misc attrs fullSize packSize offset parent 0 next)]
      where
      attrs = if compressed then 0x2 else 0x1
    Branch n (swizzle -> (first, bs)) ->
      (n, E.EN name 0 attrs 0 0 0 parent first next) : flats n bs
      where
      attrs = 0x400 -- directory

coalesceAndNumber
  :: [Handle]
  -> Handle
  -> DirectoryTrees
  -> IO EntryTree
coalesceAndNumber ins0 out (DTs m cmbs)
  | m > sizeofArray ins = fail "coalesceAndNumber: insufficient input handles"
  | otherwise = evalStateT (bitraverse d f cmbs) (-1)
  where
  ins = arrayFromList ins0

  d _ = state \n -> (n, n+1)

  f (j, fi) = StateT \n -> do
    let hj = indexArray ins j
    hSeek hj AbsoluteSeek . fromIntegral $ offset fi
    cs <- L.hGet hj . fromIntegral $ packSize fi
    off <- fromIntegral <$> hTell out
    L.hPut out cs
    let bs = if compressed fi then decompress cs else cs
        CRC32 cks = digest $ L.toStrict bs
    pure ((n, FI (compressed fi) cks (fullSize fi) (packSize fi) off), n+1)

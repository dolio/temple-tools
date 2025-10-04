module Temple.Dat.Entry
  ( Entry (..)
  , isCompressed
  , isDirectory
  , getEntries
  , writeEntries
  ) where

import Control.Monad
import Data.Bits ((.&.))
import Data.ByteString as B
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word
import System.IO

-- File table entry data for a DAT archive.
data Entry
  = EN
  { name    :: !ByteString -- file name
  , misc        :: !Word32 -- extra field, normally garbage data
  , attributes  :: !Word32 -- some attributes 0x400 directory, 0x2 compressed
  , fullSize    :: !Word32 -- original size on disk
  , packSize    :: !Word32 -- size in archive
  , offset      :: !Word32 -- offset in archive file
  , parent      :: !Word32 -- parent entry
  , firstChild  :: !Word32 -- first child entry
  , nextSibling :: !Word32 -- next sibling entry
  } deriving (Show)

-- Tests an entry's attributes against a mask
testMask :: Word32 -> Entry -> Bool
testMask mask = (mask ==) . (.&. mask) . attributes

-- Tests if an Entry's data is compressed.
isCompressed :: Entry -> Bool
isCompressed = testMask 0x2

-- Tests if an Entry is a directory
isDirectory :: Entry -> Bool
isDirectory = testMask 0x400

-- Parses a single entry in the table. Entries are variable length due to the
-- name.
parseEntry :: Get Entry
parseEntry = do
  nameLength <- getWord32le
  when (nameLength > 260) $
    fail "entry name too long"
  when (nameLength < 1) $
    fail "entry name too short"
  name <- getByteString . fromIntegral $ nameLength - 1
  guard . (== 0) =<< getWord8 -- null terminator
  -- Apparently the pointer to the name in the dat creator. They must have
  -- written the name and then just dumped the entry struct.
  misc <- getWord32le
  -- remainder of structure
  attributes <- getWord32le
  fullSize <- getWord32le
  packSize <- getWord32le
  offset <- getWord32le
  parent <- getWord32le
  firstChild <- getWord32le
  nextSibling <- getWord32le
  pure $ EN { .. }

-- Parses the entry table, which consists of a size count followed by that many
-- entries.
parseEntries :: Get [Entry]
parseEntries = do
  count <- getWord32le
  replicateM (fromIntegral count) parseEntry

-- Reads the entry table from a handle. The provided integer should be
-- sufficient to read in the entire table into memory for parsing.
getEntries :: Handle -> Int -> IO [Entry]
getEntries h sz = hGet h sz >>= \bs -> case runGet parseEntries bs of
  Left msg -> fail msg
  Right es -> pure es

putEntry :: Entry -> PutM Word32
putEntry (EN {..}) = do
  let sz = fromIntegral $ B.length name + 1
  putWord32le sz
  putByteString name
  putWord8 0 -- null terminator
  putWord32le misc
  putWord32le attributes
  putWord32le fullSize
  putWord32le packSize
  putWord32le offset
  putWord32le parent
  putWord32le firstChild
  putWord32le nextSibling
  pure sz

putEntries :: [Entry] -> PutM Word32
putEntries es = do
  putWord32le (fromIntegral $ Prelude.length es)
  puts 0 es
  where
  puts !acc [] = pure acc
  puts !acc (e:es) =
    putEntry e >>= \n -> puts (n+acc) es

writeEntries :: Handle -> [Entry] -> IO Word32
writeEntries h es
  | (namesSize, bs) <- runPutM (putEntries es)
  = namesSize <$ hPut h bs

module Temple.Dat.Entry
  ( Entry (..)
  , isCompressed
  , isDirectory
  , getEntries
  ) where

import Control.Monad
import Data.Bits ((.&.))
import Data.ByteString
import Data.Serialize.Get
import Data.Word
import System.IO

-- File table entry data for a DAT archive.
data Entry
  = EN
  { name :: ByteString    -- file name
  , attributes :: Word32  -- some attributes 0x400 directory, 0x2 compressed
  , fullSize :: Word32    -- original size on disk
  , packSize :: Word32    -- size in archive
  , offset :: Word32      -- offset in archive file
  , parent :: Word32      -- parent entry
  , firstChild :: Word32  -- first child entry
  , nextSibling :: Word32 -- next sibling entry
  }

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
  getWord32le -- ignored
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

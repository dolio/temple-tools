module Temple.Dat.Footer
  ( Footer (..)
  , writeFooter
  , readFooter
  , createFooter
  ) where

import Data.Bits
import Data.ByteString
import Data.Serialize.Get
import Data.Serialize.Put
import Data.UUID
import Data.UUID.V4
import Data.Word
import System.IO

-- Footer infor for a DAT file. The footer specifies the DAT version and a
-- file offset for the entries table. Version 1 also has a GUID, but we
-- don't actually care.
data Footer
  = FO
  { tableOffset :: Word32 -- ^ offset of the entries table
  , version :: Maybe UUID -- ^ DAT file version indicator with GUID
  }

-- If you made a Word32 with bytes to spell "DAT " or "DAT1", then encoded it
-- little endian, this is what you'd see.
detectVersion :: ByteString -> Get Word32
detectVersion " TAD" = pure 0
detectVersion "1TAD" = pure 1
detectVersion _ = fail "unrecognized DAT version"

-- cereal parser for `Footer`, needs 12 bytes
getFooterInfo :: Get (Word32, Word32)
getFooterInfo = do
  -- First four bytes indicate the format version.
  ver <- detectVersion =<< getByteString 4
  -- ignored/unknown
  getWord32host
  -- Offset of the file entries table
  off <- getWord32le
  pure (ver, off)

getGUID :: Get UUID
getGUID = fromWords64 <$> getScramble <*> getWord64be
  where
  getScramble = assemble <$> getWord32le <*> getWord16le <*> getWord16le

  assemble i j k
    =   fromIntegral i `shiftL` 32
    .|. fromIntegral j `shiftL` 16
    .|. fromIntegral k

putGUID :: UUID -> Put
putGUID (toWords64 -> (disassemble -> (i, j, k), n)) = do
  putWord32le i
  putWord16le j
  putWord16le k
  putWord64be n

disassemble :: Word64 -> (Word32, Word16, Word16)
disassemble u =
  ( fromIntegral (u .>>. 32 .&. 0xffffffff)
  , fromIntegral (u .>>. 16 .&. 0xffff)
  , fromIntegral (u .&. 0xffff)
  )

-- Read a `Footer` from a `Handle` at the correct position. In a DAT file, the
-- footer is the last 12 bytes of the file.
readFooter :: Handle -> IO Footer
readFooter h = hGet h 12 >>= \bs -> case runGet getFooterInfo bs of
  Left msg -> fail msg
  Right (0, off) -> pure $ FO off Nothing
  Right (1, off) -> do
    hSeek h RelativeSeek (-28)
    bs <- hGet h 16
    case runGet getGUID bs of
      Left msg -> fail msg
      Right guid -> pure $ FO off (Just guid)
  Right _ -> fail "Unrecognized DAT version"

putFooter :: Footer -> Put
putFooter (FO off (Just guid)) = do
  putGUID guid
  putByteString "1TAD"
  putWord32le 0
  putWord32le off
putFooter (FO off Nothing) = do
  putByteString " TAD"
  putWord32le 0
  putWord32le off

writeFooter :: Handle -> Footer -> IO ()
writeFooter h f = hPut h . runPut $ putFooter f

createFooter :: Word32 -> IO Footer
createFooter off = FO off . Just <$> nextRandom

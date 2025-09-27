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
import Data.Traversable (for)
import Data.UUID
import Data.UUID.V4
import Data.Word
import System.IO

-- Footer infor for a DAT file. The footer specifies the DAT version and a
-- file offset for the entries table. Version 1 also has a GUID, but we
-- don't actually care.
data Footer g
  = FO
  { tableOffset :: Word32  -- ^ offset of the entries table
  , namesSize   :: Word32  -- ^ total size of the names in the entries table
  , version     :: Maybe g -- ^ DAT file version indicator with GUID
  } deriving (Functor, Foldable, Traversable)

-- If you made a Word32 with bytes to spell "DAT " or "DAT1", then encoded it
-- little endian, this is what you'd see.
detectVersion :: ByteString -> Get (Maybe ())
detectVersion " TAD" = pure Nothing
detectVersion "1TAD" = pure (Just ())
detectVersion _ = fail "unrecognized DAT version"

-- cereal parser for `Footer`, needs 12 bytes
getFooterInfo :: Get (Footer ())
getFooterInfo = do
  -- First four bytes indicate the format version.
  version <- detectVersion =<< getByteString 4
  -- ignored/unknown
  namesSize <- getWord32host
  -- Offset of the file entries table
  tableOffset <- getWord32le
  pure $ FO {..}

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
readFooter :: Handle -> IO (Footer UUID)
readFooter h = hGet h 12 >>= \bs -> case runGet getFooterInfo bs of
  Left msg -> fail msg
  Right foot ->
    -- read the GUID if version 1
    for foot $ \() -> do
      hSeek h RelativeSeek (-28)
      bs <- hGet h 16
      case runGet getGUID bs of
        Left msg -> fail msg
        Right guid -> pure guid

putFooter :: Footer UUID -> Put
putFooter (FO {..}) = do
  case version of
    Nothing -> putByteString " TAD"
    Just guid -> putGUID guid *> putByteString "1TAD"
  putWord32le namesSize
  putWord32le tableOffset

writeFooter :: Handle -> Footer UUID -> IO ()
writeFooter h f = hPut h . runPut $ putFooter f

createFooter :: Word32 -> Word32 -> IO (Footer UUID)
createFooter off size = FO off size . Just <$> nextRandom

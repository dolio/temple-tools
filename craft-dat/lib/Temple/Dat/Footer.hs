module Temple.Dat.Footer where

import Data.ByteString
import Data.Serialize.Get
import Data.Word
import System.IO

-- Footer infor for a DAT file. The footer specifies the DAT version and a
-- file offset for the entries table. Version 1 also has a GUID, but we
-- don't actually care.
data Footer
  = FO
  { tableOffset :: Word32 -- ^ offset of the entries table
  , version :: Word32     -- ^ DAT file version (0 or 1)
  }

-- If you made a Word32 with bytes to spell "DAT " or "DAT1", then encoded it
-- little endian, this is what you'd see.
detectVersion :: ByteString -> Get Word32
detectVersion " TAD" = pure 0
detectVersion "1TAD" = pure 1
detectVersion _ = fail "unrecognized DAT version"

-- cereal parser for `Footer`, needs 12 bytes
parseFooter :: Get Footer
parseFooter = do
  -- First four bytes indicate the format version.
  ver <- detectVersion =<< getByteString 4
  -- ignored/unknown
  getWord32host
  -- Offset of the file entries table
  off <- getWord32le
  pure $ FO off ver

-- Read a `Footer` from a `Handle` at the correct position. In a DAT file, the
-- footer is the last 12 bytes of the file.
getFooter :: Handle -> IO Footer
getFooter h = hGet h 12 >>= \bs -> case runGet parseFooter bs of
  Left msg -> fail msg
  Right f -> pure f


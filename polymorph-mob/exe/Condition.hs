
module Condition
  ( elfHash
  , readConditionFile
  ) where

import Data.ByteString as BS
import Data.Bits
import Data.Char
import Data.Map as M
import Data.Set as S
import Data.Void
import Data.Word
import System.Directory
import System.FilePath

import Text.Megaparsec
import Text.Megaparsec.Byte

-- This is the hash function used to turn condition names into map keys.
elfHash :: BS.ByteString -> Word32
elfHash = BS.foldl' f 0 where
  lowA = fromIntegral $ ord 'a'
  lowZ = fromIntegral $ ord 'z'

  cap w | lowA <= w, w <= lowZ = w - 32
        | otherwise = w

  f h (fromIntegral . cap -> c)
    | h <- h .<<. 4 + c
    , high <- h .&. 0xf0000000
    , h <- if high /= 0 then h .^. high .>>. 24 else h
    = h .&. complement high

conditionFileParser :: Parsec Void ByteString [ByteString]
conditionFileParser = sepEndBy (takeWhileP Nothing p) eol where
  enders = S.fromList $ BS.unpack "\r\n"
  p w = w `S.notMember` enders

readConditionFile :: IO (Map Word32 BS.ByteString)
readConditionFile = happyPath <|> pure M.empty
  where
  f name = (elfHash name, name)

  happyPath = do
    fp <- getXdgDirectory XdgData ("polymorph-mob" </> "condition-names")
    bs <- BS.readFile fp
    case runParser conditionFileParser fp bs of
      Left _ -> pure M.empty
      Right m -> pure . M.fromList $ f <$> m

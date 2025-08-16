
module Decode (decodeMob) where

import Control.Monad (guard, when, replicateM)
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as L
import Data.Bits
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.UUID
import Data.Word

import Temple.Objects.Spec

import Mob

newtype Bitmap = BM BS.ByteString

countSet :: Bitmap -> Int
countSet (BM bs) = BS.foldl' (\n i -> n + popCount i) 0 bs

-- Gets the object flags set in order in the bitmap.
--
-- Note: the bitmap starts with location, which is 1, not 0 in the object
-- fields list.
setFields :: Bitmap -> [ObjectField]
setFields (BM bs) = BS.foldr thread (const []) bs 0
  where
  thread b k !n
    | b == 0 = k (n+8)
    | otherwise = fmap (fielded . (+n)) (filter (testBit b) [0..7]) ++ k (n+8)

  fielded :: Int -> ObjectField
  fielded i = toEnum $ i + 1

-- How many 32-bit blocks does it take to encode a size `n` bitmap
bits2blocks :: Int -> Int
bits2blocks n | (d,m) <- divMod n 32 = d + if m > 0 then 1 else 0

type2blocks :: ObjectType -> Int
type2blocks ty
  = bits2blocks
  . maximum
  . fmap fromEnum
  . filter (hasField ty)
  . takeWhile (< ExtraF minBound) -- no extra fields
  $ [minBound .. maxBound]

getMagic :: Get ()
getMagic = getWord32le >>= guard . (== 0x77)

-- UUIDs in a MOB file seem to be prefixed by 64 bits indicating their
-- "variant". This is massive overkill, since there are only like 4 variants.
--
-- Many UUIDs are specified as variant 2 from Microsoft, which have the bytes
-- in an odd order, and it seems all UUIDs are actually stored that way. The
-- first 8 bytes are broken into little endian values, and then the last 8
-- bytes are considered an array, which is equivalent to a big endian Word64.
-- Hence the weird arithmetic below.
getVUUID :: Get VUUID
getVUUID = do
  variant <- getWord64le
  uuid <- fromWords64 <$> getScramble <*> getWord64be
  pure $ VUUID { .. }
  where
  getScramble = assemble <$> getWord32le <*> getWord16le <*> getWord16le

  assemble i j k
    =   fromIntegral i `shiftL` 32
    .|. fromIntegral j `shiftL` 16
    .|. fromIntegral k

getMob :: Get Mob
getMob = do
  getMagic
  pad0 <- getWord16le
  compat <- getWord32le
  pad1 <- getWord16le
  protoId <- getWord32le
  skip 12
  vuuid <- getVUUID
  objType <- getObjectType
  numProps <- getWord16le
  bitmap <- getBitmap objType
  when (fromIntegral numProps /= countSet bitmap)
    (fail "validation failed: numProps does not match actual bits set")
  fields <- getFields $ setFields bitmap
  pure $ Mob {..}

getObjectType :: Get ObjectType
getObjectType = getWord32le >>= \case
  n | n <= 16 -> pure . toEnum $ fromIntegral n
    | otherwise -> fail $ "objectType out of bounds: " ++ show n

getBitmap :: ObjectType -> Get Bitmap
getBitmap ty = BM <$> getByteString (type2blocks ty * 4)

dummyByte :: String -> Get ()
dummyByte name = do
  b <- getWord8
  when (b == 0) . fail $ "bad dummy byte for " ++ name ++ " field"

getFieldValue :: String -> FieldType -> Get Value
getFieldValue name = \case
  W32F        -> W32 <$> getWord32le
  LocF        -> do dummyByte name ; Loc <$> getLoc
  W64F        -> do dummyByte name ; W64 <$> getWord64le
  I32F        -> I32 <$> getInt32le
  B32F        -> B32 . (==0xffffffff) <$> getWord32le
  F32F        -> F32 <$> getFloatle
  ObjF        -> do dummyByte name ; UID <$> getVUUID
  W32ArrF     -> W32Arr <$> getArray name 4 getWord32le
  W64ArrF     -> W64Arr <$> getArray name 8 getWord64le
  ObjArrF     -> ObjArr <$> getArray name 24 getVUUID
  ScriptArrF  -> ScriptArr <$> getArray name 12 getScriptInfo
  AbilityArrF -> I32Arr <$> getArray name 4 getInt32le
  StandptArrF -> StandptArr <$> getStandpointArray
  WayptArrF   -> WayptArr <$> getWaypointArray name
  ty          -> fail $ "unsupported field type: " ++ show ty

getArray :: String -> Word32 -> Get a -> Get [a]
getArray name exSize elem = do
  dummyByte name
  fieldSize <- getWord32le
  when (fieldSize /= exSize) . fail $
    "unexpected field size for " ++ name ++ " array: " ++ show fieldSize
  numFields <- getWord32le
  _sarc <- getWord32le
  elems <- replicateM (fromIntegral numFields) elem
  -- TODO: might be information to check in these blocks
  padBlocks <- getWord32le
  elems <$ skip (4 * fromIntegral padBlocks)

-- For some reason, there is a lot of structure to these but it is mostly
-- encoded as if it were a Word64 array.
getStandpointArray :: Get [Standpoint]
getStandpointArray = do
  dummyByte "standpoint"
  fieldSize <- getWord32le
  when (fieldSize /= 8) . fail $
    "unexpected field size for standpoint array: " ++ show fieldSize
  words <- getWord32le
  let (numFields, rem) = divMod words 10
  when (rem /= 0) . fail $
    "expected number of words for standpoint array not multiple of 10: " ++
      show words
  _sarc <- getWord32le
  elems <- replicateM (fromIntegral numFields) getStandpoint
  -- TODO: might be information to check in these blocks
  padBlocks <- getWord32le
  elems <$ skip (4 * fromIntegral padBlocks)

-- This one seems to have an even weirder structure
getWaypointArray :: String -> Get WaypointArr
getWaypointArray name = do
  dummyByte name
  fieldSize <- getWord32le
  when (fieldSize /= 8) . fail $
    "unexpected field size for waypoint array: " ++ show fieldSize
  (entries, extra) <- flip divMod 8 <$> getWord32le
  when (extra /= 2) . fail $
    "unexpected number of words for waypoint array: " ++
    show (8*entries + extra)
  _sarc <- getWord32le
  numWaypoints <- getWord32le
  dummy1 <- getWord32le
  dummy2 <- getWord32le
  dummy3 <- getWord32le
  elems <- replicateM (fromIntegral entries) getWaypoint
  -- TODO: might be information to check in these blocks
  padBlocks <- getWord32le
  skip (4 * fromIntegral padBlocks)
  pure $ Waypts numWaypoints dummy1 dummy2 dummy3 elems

getScriptInfo :: Get (Word32, Word32, Word32)
getScriptInfo = (,,) <$> getWord32le <*> getWord32le <*> getWord32le

getStandpoint :: Get Standpoint
getStandpoint = do
  sp <- Stdpt <$> getWord64le <*> getLoc <*> getOffsets <*> getWord64le
  sp <$ skip 48

getWaypoint :: Get Waypoint
getWaypoint =
  Waypt <$> getWord32le -- flags
        <*> getLoc      -- location
        <*> getOffsets  -- offsets
        <*> getFloatle  -- rotation
        <*> getWord64be -- anim index bytes in order
        <*> getWord32le -- delay
        <*> replicateM 7 getWord32le

getLoc :: Get Loc
getLoc = L <$> getInt32le <*> getInt32le

getOffsets :: Get Offsets
getOffsets = Off <$> getFloatle <*> getFloatle

-- Given a sequence of fields in the order they will occur, reads their values
-- into a map.
getFields :: [ObjectField] -> Get (Map ObjectField Value)
getFields fs = do Map.fromList <$> traverse getField fs

-- Supported fields for a mob file
getField :: ObjectField -> Get (ObjectField, Value)
getField fl = (,) fl <$> getFieldValue (fieldName fl) (fieldType fl)

decodeMob :: L.ByteString -> Either String Mob
decodeMob bs = case runGetOrFail getMob bs of
  Left (_, _, msg) -> Left msg
  Right (_, _, mob) -> Right mob

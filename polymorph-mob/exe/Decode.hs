
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

import Temple.Object.Field
import Temple.Object.Field.Type
import Temple.Object.Script
import Temple.Object.Type

import Bitmap
import Mob

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
--
-- In actual ToEE files, it seems like all UUIDs are variant 2, but some have
-- the first 6 bytes filled with hex `cd`. I have no idea what this indicates.
getObjectId :: Get ObjectId
getObjectId = do
  variant <- getWord64le
  uuid <- fromWords64 <$> getScramble <*> getWord64be
  pure $ ObjId { .. }
  where
  getScramble = assemble <$> getWord32le <*> getWord16le <*> getWord16le

  assemble i j k
    =   fromIntegral i `shiftL` 32
    .|. fromIntegral j `shiftL` 16
    .|. fromIntegral k

getObjectInfo :: Get ObjectInfo
getObjectInfo = do
  subtype <- getWord16le
  skip 6 -- padding
  protoId <- getWord32le
  skip 12 -- padding
  pure $ ObjInfo {..}

-- Finds the number of bits necessary to store the fields of a given object
-- type.
type2bits :: ObjectType -> Int
type2bits ty
  = maximum
  . fmap fromEnum
  . filter (hasField ty)
  . takeWhile (< ExtraF minBound) -- no extra fields
  $ [minBound .. maxBound]

getMob :: Get Mob
getMob = do
  getMagic
  objInfo <- isolate 24 $ getObjectInfo
  objId <- isolate 24 $ getObjectId
  objType <- getObjectType
  fields <- getFields objType
  isEmpty >>= \b -> when (not b) $
    fail "extra bytes at end"
  pure $ Mob {..}

getObjectType :: Get ObjectType
getObjectType = getWord32le >>= \case
  n | n <= 16 -> pure . toEnum $ fromIntegral n
    | otherwise -> fail $ "objectType out of bounds: " ++ show n

-- Every field type larger than 32 bits has a short circuit byte. If this is
-- 0, they skip encoding 'null' values in the full format. Presumably this is
-- because it takes a fair amount (by early 2000s standards) more space to
-- encode an empty array than just a single byte (and empty arrays might be
-- relatively common).
shortCircuit :: a -> Get a -> Get a
shortCircuit dflt full = getWord8 >>= \case
  0 -> pure dflt
  _ -> full

getFieldValue :: String -> FieldType -> Get Value
getFieldValue name = \case
  W32F        -> W32 <$> getWord32le
  LocF        -> shortCircuit Null $ Loc <$> getLoc
  W64F        -> shortCircuit Null $ W64 <$> getWord64le
  I32F        -> I32 <$> getInt32le
  B32F        -> B32 . (==0xffffffff) <$> getWord32le
  F32F        -> F32 <$> getFloatle
  ObjF        -> shortCircuit Null $ Obj <$> getObjectId
  W32ArrF     -> shortCircuit Null $ W32Arr <$> getArray name 4 getWord32le
  W64ArrF     -> shortCircuit Null $ W64Arr <$> getArray name 8 getWord64le
  ObjArrF     -> shortCircuit Null $ ObjArr <$> getArray name 24 getObjectId
  ScriptArrF  -> ScriptArr <$> shortCircuit Map.empty getScriptArray
  AbilityArrF -> shortCircuit Null $ I32Arr <$> getArray name 4 getInt32le
  StandptArrF -> shortCircuit Null $ StandptArr <$> getStandpointArray
  WayptArrF   -> shortCircuit Null $ WayptArr <$> getWaypointArray
  StringF     -> shortCircuit Null $ String <$> getString
  ty          -> fail $ "unsupported field type: " ++ show ty

getScriptArray :: Get (Map ObjectScript Script)
getScriptArray = do
  fieldSize <- getWord32le
  when (fieldSize /= 12) . fail $
    "unexpected field size for script array: " ++ show fieldSize
  numFields <- getWord32le
  _bitmapIdx <- getWord32le
  let i = fromIntegral numFields
  array i <$> replicateM i getScriptInfo <*> getArrayBitmap >>= \case
    Dense scs -> pure $ Map.fromList $ zip [minBound ..] scs
    Sparse scs bm
      | countSet bm == i ->
        pure . Map.fromList $ zip (setFields minBound bm) scs
      | otherwise ->
        fail "bitmap for script array doesn't match number of scripts"

getArray :: String -> Word32 -> Get a -> Get (Array a)
getArray name exSize elem = do
  fieldSize <- getWord32le
  when (fieldSize /= exSize) . fail $
    "unexpected field size for " ++ name ++ " array: " ++ show fieldSize
  numFields <- getWord32le
  _bitmapId <- getWord32le
  let i = fromIntegral numFields
  array i <$> replicateM i elem <*> getArrayBitmap

getString :: Get BS.ByteString
getString = do
  size <- getWord32le
  getByteString (fromIntegral size) <* skip 1 -- presumably null terminator

-- For some reason, there is a lot of structure to these but it is mostly
-- encoded as if it were a Word64 array.
getStandpointArray :: Get (Array Standpoint)
getStandpointArray = do
  fieldSize <- getWord32le
  when (fieldSize /= 8) . fail $
    "unexpected field size for standpoint array: " ++ show fieldSize
  words <- getWord32le
  let (numFields, rem) = divMod words 10
  when (rem /= 0) . fail $
    "expected number of words for standpoint array not multiple of 10: " ++
      show words
  _bitmapIdx <- getWord32le
  array (fromIntegral words)
    <$> replicateM (fromIntegral numFields) getStandpoint
    <*> getArrayBitmap

-- This one seems to have an even weirder structure
getWaypointArray :: Get WaypointArr
getWaypointArray = do
  fieldSize <- getWord32le
  when (fieldSize /= 8) . fail $
    "unexpected field size for waypoint array: " ++ show fieldSize
  words <- getWord32le
  let (entries, extra) = divMod words 8
  when (extra /= 2) . fail $
    "unexpected number of words for waypoint array: " ++
    show (8*entries + extra)
  _bitmapIdx <- getWord32le
  wayptCount <- getWord32le
  wayptExtra1 <- getWord32le
  wayptExtra2 <- getWord32le
  wayptExtra3 <- getWord32le
  waypts <- replicateM (fromIntegral entries) getWaypoint
  bitmap <- getArrayBitmap
  when (not $ isDense (fromIntegral words) bitmap) . fail $
    "expected waypoint array to have a dense bitmap"
  pure $ Waypts {..}

getArrayBitmap :: Get Bitmap
getArrayBitmap = do
  size <- getWord32le
  getBitmapBlocks $ fromIntegral size

getScriptInfo :: Get Script
getScriptInfo = Script <$> getWord32le <*> getWord32le <*> getWord32le

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
        <* skip 28

getLoc :: Get Loc
getLoc = L <$> getInt32le <*> getInt32le

getOffsets :: Get Offsets
getOffsets = Off <$> getFloatle <*> getFloatle

-- Reads the main section of a mob, containing its fields.
getFields :: ObjectType -> Get (Map ObjectField Value)
getFields objType = do
  numProps <- getWord16le
  bitmap <- getBitmap $ type2bits objType
  when (fromIntegral numProps /= countSet bitmap)
    (fail "validation failed: numProps does not match actual bits set")
  Map.fromList <$> traverse getField (setFields (GeneralF Location) bitmap)

-- Supported fields for a mob file
getField :: ObjectField -> Get (ObjectField, Value)
getField fl = (,) fl <$> getFieldValue (fieldName fl) (fieldType fl)

decodeMob :: L.ByteString -> Either String Mob
decodeMob bs = case runGetOrFail getMob bs of
  Left (_, _, msg) -> Left msg
  Right (_, _, mob) -> Right mob

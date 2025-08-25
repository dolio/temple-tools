
module Decode (decodeMob, decodeMobs, decodeDiffs) where

import Control.Applicative ((<|>))
import Control.Monad (when, replicateM)
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as L
import Data.Bits
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.UUID
import Data.Word
import Numeric (showHex)

import Temple.Object.Field
import Temple.Object.Field.Type
import Temple.Object.Script
import Temple.Object.Type

import Bitmap
import Mob

-- This gets a magic number from a file. This is a 32-bit sequence that is
-- expected to match a specific value. If it doesn't, then an error displaying
-- the expected vs. actual value occurs.
getMagic :: Word32 -> Get ()
getMagic target = do
  n <- getWord32le
  if n == target
  then pure ()
  else do
    k <- bytesRead
    fail $ "unrecognized magic number " ++ mismatch n k
  where
  mismatch m k = "0x" ++ showHex m " expected 0x" ++ showHex target " at position 0x" ++ showHex k ""

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

-- The object info in a mob file contains a subtype specifier and a prototype
-- number. The rest seems to be padding.
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
  getMagic 0x77
  objInfo <- getObjectInfo
  objId <- isolate 24 getObjectId
  objType <- getObjectType
  fields <- getFields MobFile objType
  pure $ Mob {..}

getMobDiff :: Mob -> Get MobDiff
getMobDiff mob = do
  getMagic 0x77
  getMagic 0x12344321
  oid <- getObjectId
  when (oid /= objId mob) $
    fail $ "diff object id did not match mob: " ++ toString (uuid oid)
  MobDiff <$> getFields DiffFile (objType mob) <* getMagic 0x23455432

checkEOF :: Get ()
checkEOF = isEmpty >>= \b -> when (not b) do
  n <- bytesRead
  fail $ "extra bytes at position " ++ show n

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
  CondArrF    -> shortCircuit Null $ CondArr <$> getArray name 4 getWord32le
  ScriptArrF  -> ScriptArr <$> shortCircuit Map.empty getScriptArray
  AbilityArrF -> shortCircuit Null $ I32Arr <$> getArray name 4 getInt32le
  StandptArrF -> shortCircuit Null $ StandptArr <$> getStandpointArray
  WayptArrF   -> shortCircuit Null $ WayptArr <$> getWaypointArray
  SpellArrF   -> shortCircuit Null $ SpellArr <$> getArray name 32 getSpellData
  StringF     -> shortCircuit Null $ String <$> getString
  ty          -> fail $ "unsupported field type: " ++ name ++ " : " ++ show ty

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

getSpellData :: Get SpellData
getSpellData = do
  spellEnum <- getWord32le
  spellClass <- getWord32le
  spellLevel <- getWord32le
  spellType <- getSpellType
  spellUsed <- (/= 0) <$> getWord8
  skip 2 -- padding
  spellMeta <- getMetaMagic
  -- these are apparently actual fields, but I don't know what the information
  -- in them actually means
  spellInd1 <- getWord32le
  spellInd2 <- getWord32le
  spellInd3 <- getWord32le
  pure $ Spell {..}

getSpellType :: Get SpellType
getSpellType = getWord8 >>= \case
  0 -> pure SpellNone
  1 -> pure SpellKnown
  2 -> pure SpellMemorized
  3 -> pure SpellCast
  4 -> pure SpellAtWill
  n -> fail $ "unrecognized spell type: " ++ show n

-- Only the 3 bytes are relevant, the last is padding
getMetaMagic :: Get Metamagic
getMetaMagic = Mm . (.&. 0xffffff) <$> getWord32le

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
getFields :: Format -> ObjectType -> Get (Map ObjectField Value)
getFields MobFile objType = do
  numProps <- getWord16le
  bitmap <- getBitmap $ type2bits objType
  when (fromIntegral numProps /= countSet bitmap)
    (fail "validation failed: numProps does not match actual bits set")
  Map.fromList <$> traverse getField (setFields (GeneralF Location) bitmap)
getFields DiffFile objType = do
  bitmap <- getBitmap $ type2bits objType
  Map.fromList <$> traverse getField (setFields (GeneralF Location) bitmap)

-- Supported fields for a mob file
getField :: ObjectField -> Get (ObjectField, Value)
getField fl = (,) fl <$> getFieldValue (fieldName fl) (fieldType fl)

runGetEither :: Get a -> L.ByteString -> Either String a
runGetEither get bs = case runGetOrFail get bs of
  Left  (_, _, message) -> Left message
  Right (_, _,  result) -> Right result

decodeMob :: L.ByteString -> Either String Mob
decodeMob = runGetEither $ getMob <* checkEOF

untilEOF :: Get a -> Get [a]
untilEOF p = [] <$ checkEOF <|> (:) <$> p <*> untilEOF p

decodeMobs :: L.ByteString -> Either String [Mob]
decodeMobs = runGetEither $ untilEOF getMob

decodeDiffs
  :: Map UUID Mob -> L.ByteString -> Either String [(ObjectId, MobDiff)]
decodeDiffs mobs = runGetEither $ getItems [] <* checkEOF
  where
  tryGetId = Just <$> getObjectId <|> pure Nothing
  getItems acc = tryGetId >>= \case
    Nothing -> pure $ reverse acc
    Just oid -> case Map.lookup (uuid oid) mobs of
      Just mob -> do
        p <- (,) oid <$> getMobDiff mob
        getItems (p:acc)
      Nothing -> fail $ "could not find mob: " ++ name
        where name = objectIdToFileName oid

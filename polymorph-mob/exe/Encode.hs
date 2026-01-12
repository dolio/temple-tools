
module Encode where

import Control.Monad (replicateM_)
import Data.Binary.Put
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as L
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.UUID
import Data.Word

import Bitmap
import Mob

import Temple.Object.Field
import Temple.Object.Field.Type
import Temple.Object.Script
import Temple.Object.Skill
import Temple.Object.Type

-- magic version number
putMagic :: Put
putMagic = putWord32le 0x77

-- Puts a UUID in a format appropriate to MOB files.
--
-- I haven't seen any UUIDs that aren't encoded in Microsoft's GUID (variant
-- 2) format, which breaks the first 8 bytes into 1 4-byte and 2 2-byte
-- values, and encodes them little endian. I've also only seen two variant
-- numbers:
--
--   2
--   0xcdcdcdcdcdcd0002
--
-- Presumably the latter are also being specified as variant 2, though I don't
-- know what the first 6 bytes are meant to indicate.
putObjectId :: ObjectId -> Put
putObjectId (ObjId {..}) = do
  putWord64le variant
  case toWords64 uuid of
    (u0, u1) -> do
      putWord32le . fromIntegral $ u0 .>>. 32 .&. 0xffffffff
      putWord16le . fromIntegral $ u0 .>>. 16 .&. 0xffff
      putWord16le . fromIntegral $ u0 .&. 0xffff
      putWord64be u1

putMob :: Mob -> Put
putMob (Mob {..}) = do
  putMagic
  putObjectInfo objInfo
  putObjectId objId
  putObjectType objType
  putFields objType fields

mobBitmap :: ObjectType -> Map ObjectField Value -> Bitmap
mobBitmap ty fields = fromFields (GeneralF Location) n (Map.keysSet fields)
  where
  tweak (flip divMod 8 -> (d, m)) = d + if m == 0 then 0 else 1

  n = tweak
    . maximum
    . fmap fromEnum
    . filter (hasField ty)
    . takeWhile (< ExtraF minBound)
    $ [minBound .. maxBound]

-- Determines how many bytes a written `Mob` will consume
mobSize :: Mob -> Word32
mobSize (Mob {..}) = fromIntegral $ 56 + metaSz + dataSz where
  bm = mobBitmap objType fields
  metaSz = 2 + 4 * countBlocks bm
  dataSz = foldl' (\s v -> s + valueSize v) 0 fields

putObjectType :: ObjectType -> Put
putObjectType = putWord32le . fromIntegral . fromEnum

putObjectInfo :: ObjectInfo -> Put
putObjectInfo (ObjInfo {..}) = do
  putWord16le subtype
  putWord16le 0 ; putWord32le 0 -- padding
  putWord32le protoId
  replicateM_ 3 $ putWord32le 0 -- more padding

putFields :: ObjectType -> Map ObjectField Value -> Put
putFields ty fields = do
  putWord16le . fromIntegral $ countSet bm
  putBitmap bm
  traverse_ (uncurry putField) $ Map.toList fields
  where
  bm = mobBitmap ty fields

putField :: ObjectField -> Value -> Put
putField f = putFieldByType (fieldName f) (fieldType f)

valueSize :: Value -> Int
valueSize = \case
  Null -> 1
  W32 _ -> 4
  Loc _ -> 9
  W64 _ -> 9
  I32 _ -> 4
  F32 _ -> 4
  B32 _ -> 4
  Obj _ -> 25
  I32Arr a -> arraySize 4 a
  W32Arr a -> arraySize 4 a
  W64Arr a -> arraySize 8 a
  CondArr a -> arraySize 4 a
  ObjArr a -> arraySize 24 a
  ScriptArr m
    | Map.null m -> 1
    | otherwise -> 21 + 12 * Map.size m
  StandptArr a -> 13 + 80*ln + arrayBitmapSize bm where
    ln = length $ content a
    bm | Sparse {..} <- a = _bitmap
       | otherwise = denseBitmap (10 * ln)
  SkillArr m
    | Map.null m -> 1
    | otherwise -> 13 + 4*ln + arrayBitmapSize bm where
      ln = Map.size m
      bm = fromFields minBound 6 $ Map.keysSet m
  WayptArr wa -> 29 + 64*ln + arrayBitmapSize bm where
    ln = length $ waypts wa
    bm = denseBitmap (8*ln + 2)
  String str -> 6 + BS.length str
  SpellArr a -> arraySize 32 a

-- Calculates the bytes that will be used by writing an array bitmap
arrayBitmapSize :: Bitmap -> Int
arrayBitmapSize bm = 4 + 4 * max 2 (countBlocks bm)

arraySize :: Int -> Array e -> Int
arraySize elemSize arr = 13 + elemSize*ln + arrayBitmapSize (bitmap arr)
  where
  ln = length $ content arr

shortCircuits :: Set.Set FieldType
shortCircuits =
  Set.fromList
    [ W64F , LocF, ObjF, StringF
    , W32ArrF, W64ArrF, ObjArrF
    , StandptArrF, WayptArrF, AbilityArrF
    , ScriptArrF, SpellArrF
    ]

putBool32 :: Bool -> Put
putBool32 b = putWord32le $ if b then 0xffffffff else 0

putFieldByType :: String -> FieldType -> Value -> Put
putFieldByType name = \cases
  ty           Null | ty `elem` shortCircuits -> putWord8 0

  W32F        (W32 w)          -> putWord32le w
  I32F        (I32 i)          -> putInt32le i
  F32F        (F32 f)          -> putFloatle f
  B32F        (B32 b)          -> putBool32 b
  W64F        (W64 w)          -> putWord8 1 *> putWord64le w
  LocF        (Loc l)          -> putWord8 1 *> putLoc l
  ObjF        (Obj o)          -> putWord8 1 *> putObjectId o
  StringF     (String s)       -> putWord8 1 *> putString s
  W32ArrF     (W32Arr ws)      -> putArray 4 putWord32le ws
  W64ArrF     (W64Arr ws)      -> putArray 8 putWord64le ws
  CondArrF    (CondArr cs)     -> putArray 4 putWord32le cs
  ObjArrF     (ObjArr os)      -> putArray 24 putObjectId os
  StandptArrF (StandptArr sps) -> putStandpointArray sps
  WayptArrF   (WayptArr wps)   -> putWaypointArray wps
  AbilityArrF (I32Arr is)      -> putArray 4 putInt32le is
  AbilityArrF (W32Arr is)      -> putArray 4 putWord32le is
  ScriptArrF  (ScriptArr ss)   -> putScriptArray ss
  SkillArrF   (SkillArr sks)   -> putSkillArray sks
  SpellArrF   (SpellArr sps)   -> putArray 32 putSpellData sps
  _           vl ->
    error $ "bad value for " <> name <> ": " ++ show vl

-- This puts an ordinary array, where each serialized element corresponds to
-- an actual logical element of the array, so we can determine the number of
-- serialized elements from the length of the element list.
putArray :: Word32 -> (e -> Put) -> Array e -> Put
putArray sz pe arr = do
  putWord8 1 -- not short circuitsing
  putWord32le sz
  putWord32le . fromIntegral $ numEntries
  putWord32le 0 -- dummy bitmap index
  traverse_ pe $ content arr
  putArrayBitmap bm
  where
  bm = bitmap arr
  numEntries = length $ content arr

-- For some reason, array bitmaps always seem to be padded to at least two
-- blocks in actual mob files. Replicating that.
putArrayBitmap :: Bitmap -> Put
putArrayBitmap bm = do
  putWord32le $ fromIntegral n
  putBitmap bm
  replicateM_ (n-c) $ putWord32le 0
  where
  c = countBlocks bm
  n = max 2 c

putWaypointArray :: WaypointArr -> Put
putWaypointArray (Waypts {..}) = do
  putWord8 1                         -- no short circuit
  putWord32le 8                      -- field size
  putWord32le $ fromIntegral words   -- total number of Word64 entries
  putWord32le 0                      -- dummy bitmap index
  putWord32le wayptCount             -- the separate waypoint count
  putWord32le wayptExtra1            -- padding?
  putWord32le wayptExtra2            -- padding?
  putWord32le wayptExtra3            -- padding?
  traverse_ putWaypoint waypts
  putArrayBitmap $ denseBitmap words -- waypoint arrays are always dense
  where
  words = 8 * length waypts + 2

putWaypoint :: Waypoint -> Put
putWaypoint (Waypt {..}) = do
  putWord32le wayptFlags
  putLoc wayptLoc
  putOffsets wayptOffs
  putFloatle wayptRot
  putWord64be wayptAnims
  putWord32le wayptDelay
  replicateM_ 7 $ putWord32le 0 -- padding

putStandpointArray :: Array Standpoint -> Put
putStandpointArray arr = do
  putWord8 1 -- not short circuiting
  putWord32le 8
  putWord32le $ fromIntegral words
  putWord32le 0 -- dummy bitmap index
  traverse_ putStandpoint $ content arr
  putArrayBitmap bm
  where
  words = 10 * length (content arr)
  bm | Sparse {..} <- arr = _bitmap
     | otherwise = denseBitmap words

putStandpoint :: Standpoint -> Put
putStandpoint (Stdpt {..}) = do
  putWord64le mapInfo
  putLoc loc
  putOffsets offsets
  putWord64le jp
  replicateM_ 6 $ putWord64le 0 -- padding

putScriptArray :: Map ObjectScript Script -> Put
putScriptArray m
  | Map.null m = putWord8 0
  | otherwise = do
    putWord8 1 -- no short circuit
    putWord32le 12 -- script field size
    putWord32le . fromIntegral $ Map.size m
    putWord32le 0 -- dummy bitmap index
    traverse_ putScript $ Map.elems m
    putArrayBitmap . fromFields minBound 6 $ Map.keysSet m

putSkillArray :: Map Skill Word32 -> Put
putSkillArray m
  | Map.null m = putWord8 0
  | otherwise = do
    putWord8 1 -- no short circuit
    putWord32le 4 -- 4 byte words
    putWord32le . fromIntegral $ Map.size m
    putWord32le 0 -- dummy bitmap index
    traverse_ putWord32le $ Map.elems m
    putArrayBitmap . fromFields minBound 6 $ Map.keysSet m

putSpellData :: SpellData -> Put
putSpellData (Spell {..}) = do
  putWord32le spellEnum
  putWord32le spellClass
  putWord32le spellLevel
  putSpellType spellType
  putWord8 $ if spellUsed then 1 else 0
  putWord16le 0 -- padding
  putMetaMagic spellMeta
  putWord32le spellInd1
  putWord32le spellInd2
  putWord32le spellInd3

putSpellType :: SpellType -> Put
putSpellType ty = putWord8 case ty of
  SpellNone -> 0
  SpellKnown -> 1
  SpellMemorized -> 2
  SpellCast -> 3
  SpellAtWill -> 4

putMetaMagic :: Metamagic -> Put
putMetaMagic (Mm w) = putWord32le w

putLoc :: Loc -> Put
putLoc (L {..}) = putInt32le locx *> putInt32le locy

putOffsets :: Offsets -> Put
putOffsets (Off {..}) = putFloatle offx *> putFloatle offy

putScript :: Script -> Put
putScript (Script i j k) =
  putWord32le i *> putWord32le j *> putWord32le k

putString :: BS.ByteString -> Put
putString bs = do
  putSzString bs
  putWord8 0

putSzString :: BS.ByteString -> Put
putSzString bs = do
  putWord32le . fromIntegral $ BS.length bs
  putByteString bs

putPlayer :: Player -> Put
putPlayer (Player {..}) = do
  putWord32le pcFlags
  putWord32le $ mobSize pcData
  putObjectId pcId
  putSzString pcName
  putWord32le pcPortrait
  putWord32le pcGender
  putWord32le pcClass
  putWord32le pcRace
  putWord32le pcAlign
  putWord32le pcHp
  putMob pcData

encodeMob :: Mob -> L.ByteString
encodeMob = runPut . putMob

encodePlayer :: Player -> L.ByteString
encodePlayer = runPut . putPlayer


module Decode (decodeMob) where

import Control.Monad (guard, when)
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as L
import Data.Bits
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.UUID

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

-- For some reason, the references I can find have 24 bytes for the guid. Only
-- the last 16 bytes are the actual guid, so the first 8 must be some sort of
-- padding. The bytes are also in a strange order relative to the file name
-- (which has the textual guid).
getMobUUID :: Get UUID
getMobUUID = do
  skip 8
  u0 <- assemble <$> getWord32le <*> getWord16le <*> getWord16le
  u1 <- getWord64be
  pure $ fromWords64 u0 u1
  where
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
  uuid <- getMobUUID
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

getW32 :: Get Value
getW32 = W32 <$> getWord32le

getF32 :: Get Value
getF32 = F32 <$> getFloatle

getW32x2 :: Get Value
getW32x2 = W32x2 <$> getWord32le <*> getWord32le

getW64 :: Get Value
getW64 = W64 <$> getWord64le

getB32 :: Get Value
getB32 = B32 . (==0xffffffff) <$> getWord32le

getI32 :: Get Value
getI32 = I32 <$> getInt32le

getUID :: Get Value
getUID = UID <$> getMobUUID

-- Given a sequence of fields in the order they will occur, reads their values
-- into a map.
--
-- Note: there's a leading 'dummy byte' apparently.
getFields :: [ObjectField] -> Get (Map ObjectField Value)
getFields fs = do skip 1 ; Map.fromList <$> traverse getField fs

-- Supported fields for a mob file
getField :: ObjectField -> Get (ObjectField, Value)
getField fl = (fl,) <$> case fl of
  GeneralF Location -> getW32x2
  GeneralF XOffset -> getF32
  GeneralF YOffset -> getF32
  GeneralF Transparency -> getW32
  GeneralF ModelScale -> getW32
  GeneralF Flags -> getW32
  GeneralF Name -> getW32
  GeneralF HpPts -> getW32
  GeneralF HpAdj -> getW32
  GeneralF HpDamage -> getW32
  GeneralF ScriptsIdx -> fail "ScriptsIdx"
  GeneralF Rotation -> getF32
  GeneralF SpeedWalk -> getF32
  GeneralF SpeedRun -> getF32
  GeneralF Radius -> getF32
  GeneralF RenderHeight3D -> getF32
  GeneralF Conditions -> fail "Conditions"
  GeneralF ConditionArg0 -> fail "ConditionArg0"
  GeneralF PermanentMods -> fail "PermanentMods"
  GeneralF Dispatcher -> getB32
  GeneralF SecretdoorFlags -> getW32
  GeneralF SecretdoorEffectname -> getW32
  GeneralF SecretdoorDC -> getW32
  GeneralF ZOffset -> getF32
  GeneralF PermanentModData -> fail "PermanentModData"

  PortalF PortalFlags -> getW32
  PortalF PortalLockDC -> getW32
  PortalF PortalKeyId -> getW32
  PortalF PortalNotifyNpc -> getW32

  ContainerF ContainerFlags -> getW32
  ContainerF ContainerLockDC -> getW32
  ContainerF ContainerKeyId -> getW32
  ContainerF ContainerInventoryNum -> getW32
  ContainerF ContainerInventoryListIdx -> fail "ContainerInventoryListIdx"
  ContainerF ContainerInventorySource -> getW32
  ContainerF ContainerNotifyNpc -> getW32

  SceneryF SceneryFlags -> getW32
  SceneryF SceneryTeleportTo -> getW32

  ItemF ItemFlags -> getW32
  ItemF ItemParent -> skip 1 *> getUID
  ItemF ItemWeight -> getW32
  ItemF ItemWorth -> getW32
  ItemF ItemInvLocation -> getW32
  ItemF ItemQuantity -> getW32

  WeaponF WeaponFlags -> getW32

  AmmoF AmmoQuantity -> getW32

  ArmorF ArmorFlags -> getW32
  ArmorF ArmorAcAdj -> getI32
  ArmorF ArmorMaxDexBonus -> getI32
  ArmorF ArmorArcaneSpellFailure -> getI32
  ArmorF ArmorArmorCheckPenalty -> getI32

  MoneyF MoneyQuantity -> getW32

  KeyF KeyKeyId -> getW32

  CritterF CritterFlags -> getW32
  CritterF CritterFlags2 -> getW32
  CritterF CritterAbilitiesIdx -> fail "CritterAbilitiesIdx"
  CritterF CritterRace -> getI32
  CritterF CritterGender -> getI32
  CritterF CritterPadInt1 -> getI32
  CritterF CritterMoneyIdx -> fail "CritterMoneyIdx"
  CritterF CritterInventoryNum -> getW32
  CritterF CritterInventoryListIdx -> fail "CritterInventoryListIdx"
  CritterF CritterInventorySource -> fail "CritterInventorySource"
  CritterF CritterTeleportDest -> skip 1 *> getW32x2
  CritterF CritterTeleportMap -> getW32
  CritterF CritterReach -> getW32
  CritterF CritterPadInt4 -> getI32 -- LevelupScheme?

  NpcF NpcFlags -> getW32
  NpcF NpcWaypointsIdx -> fail "NpcWaypointsIdx"
  NpcF NpcStandpointDayINVALID -> W32 0 <$ skip 9
  NpcF NpcStandpointNightINVALID -> W32 0 <$ skip 9
  NpcF NpcFaction -> fail "NpcFaction"
  NpcF NpcSubstituteInventory -> skip 1 *> getUID
  NpcF NpcGeneratorData -> getW32
  NpcF NpcAiFlags64 -> getW64
  NpcF NpcStandpoints -> fail "NpcStandpoints"

  fld -> fail $ "getField: unsupported field: " ++ fieldName fld

decodeMob :: L.ByteString -> Mob
decodeMob = runGet getMob

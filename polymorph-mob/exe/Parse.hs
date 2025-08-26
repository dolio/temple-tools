
module Parse (readMob) where

import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.UUID
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer qualified as Lex

import Temple.Object.Field
import Temple.Object.Field.Type
import Temple.Object.Script
import Temple.Object.Skill
import Temple.Object.Type

import Condition
import Mob

type Parser = Parsec Void ByteString

data MobField
  = MobInfo
  | MobId
  | MobType
  | MobField ObjectField
  deriving (Eq, Ord, Show)

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

char8 :: Char -> Parser Word8
char8 = char . ord8

quote :: Parser Word8
quote = char8 '"'

colon :: Parser Word8
colon = lexeme $ char8 ':'

comma :: Parser Word8
comma = lexeme $ char8 ','

obrace, cbrace :: Parser Word8
obrace = lexeme $ char8 '{'
cbrace = lexeme $ char8 '}'

obrack, cbrack :: Parser Word8
obrack = lexeme $ char8 '['
cbrack = lexeme $ char8 ']'

quoted :: Parser a -> Parser a
quoted p = lexeme $ quote *> p <* quote

parseJsonFieldName :: Parser a -> Parser a
parseJsonFieldName name = quoted name <* colon

parseJsonObject :: Parser k -> (k -> Parser v) -> Parser [v]
parseJsonObject pk pv = obrace *> sepBy field comma <* cbrace
  where
  field = parseJsonFieldName pk >>= pv

parseMobObjectField :: Parser MobField
parseMobObjectField =
  choice
    [ MobInfo <$ try (chunk "mob-info")
    , MobId <$ try (chunk "mob-id")
    , MobType <$ try (chunk "mob-type")
    , MobField <$> parseFieldName
    ]

parseMob :: Parser Mob
parseMob = finalize <$> parseJsonObject parseMobObjectField \case
  MobInfo -> setMobInfo <$> parseMobInfo
  MobId -> setMobId <$> parseObjectId
  MobType -> setMobType <$> parseMobType
  MobField f -> setMobField f <$> parseValueByType (fieldType f)

parseInteger :: Num a => Parser a
parseInteger = Lex.signed space integer
  where
  integer = lexeme
          $ choice [ try (chunk "0x") *> Lex.hexadecimal, Lex.decimal ]

parseFloat :: RealFloat a => Parser a
parseFloat = Lex.signed space $ lexeme Lex.float

parseMobInfo :: Parser ObjectInfo
parseMobInfo = finalize <$> parseJsonObject parseField \case
  False -> setOInfoSubtype <$> parseInteger
  True -> setOInfoProtoId <$> parseInteger
  where
  parseField =
    choice
      [ False <$ chunk "subtype"
      , True <$ chunk "protoId"
      ]

parseObjectId :: Parser ObjectId
parseObjectId = finalize <$> parseJsonObject field \case
  False -> setObjectIdVariant <$> parseInteger
  True -> setObjectIdUUID <$> uuid
  where
  field =
    choice
      [ False <$ chunk "variant"
      , True <$ chunk "uuid"
      ]

  uuid = do
    str <- lexeme (quoted $ takeWhileP Nothing (/= fromIntegral (ord '"')))
    case fromLazyASCIIBytes str of
      Nothing -> fail "ill formatted UUID"
      Just u -> pure u

parseMobType :: Parser ObjectType
parseMobType = lexeme $ quoted parseTypeName

parseValueByType :: FieldType -> Parser Value
parseValueByType = \case
  W32F -> W32 <$> parseInteger
  W64F -> W64 <$> parseInteger
  I32F -> I32 <$> parseInteger
  F32F -> F32 <$> parseFloat
  B32F -> B32 <$> bool
  ObjF -> Obj <$> parseObjectId
  LocF -> Loc <$> parseLoc
  -- StringF -> String <$> parseString
  W32ArrF -> W32Arr . Dense <$> parseArr parseInteger
  W64ArrF -> W64Arr . Dense <$> parseArr parseInteger
  ObjArrF -> ObjArr . Dense <$> parseArr parseObjectId
  CondArrF -> CondArr . Dense <$> parseArr parseCondition
  AbilityArrF -> W32Arr . Dense <$> parseArr parseInteger
  ScriptArrF -> ScriptArr <$> parseScriptArr
  WayptArrF -> WayptArr <$> parseWaypointArr
  StandptArrF -> StandptArr . Dense <$> parseArr parseStandpoint
  SkillArrF -> SkillArr <$> parseSkillArr
  ft -> fail $ "unsupported field type: " ++ show ft
  where
  bool = (False <$ lexeme (chunk "false")) <|> (True <$ lexeme (chunk "true"))

parseArr :: Parser e -> Parser [e]
parseArr pe = obrack *> elems <* cbrack
  where elems = sepBy pe comma

parseLoc :: Parser Loc
parseLoc = finalize <$> parseJsonObject field \case
  False -> setLocX <$> parseInteger
  True -> setLocY <$> parseInteger
  where field = False <$ "locx" <|> True <$ "locy"

parseCondition :: Parser Word32
parseCondition
  = elfHash . toStrict <$> quoted (takeWhileP Nothing (/= ord8 '"'))
 <|> parseInteger

parseScript :: Parser Script
parseScript = finalize <$> parseJsonObject field \case
  0 -> setScriptUnk <$> parseInteger
  1 -> setScriptCounters <$> parseInteger
  2 -> setScriptId <$> parseInteger
  _ -> error "impossible"
  where
  field :: Parser Int
  field = 0 <$ "unknown" <|> 1 <$ "counters" <|> 2 <$ "script-id"

parseScriptArr :: Parser (Map ObjectScript Script)
parseScriptArr =
  Map.fromList <$>
    parseJsonObject parseScriptName (\k -> (,) k <$> parseScript)

parseSkillArr :: Parser (Map Skill Word32)
parseSkillArr =
  Map.fromList <$>
    parseJsonObject parseSkillName (\k -> (,) k <$> parseInteger)

parseWaypoint :: Parser Waypoint
parseWaypoint = finalize <$> parseJsonObject field \case
  0 -> setWayptFlags <$> parseInteger
  1 -> setWayptLoc <$> parseLoc
  2 -> setWayptOffs <$> parseOffsets
  3 -> setWayptRot <$> parseFloat
  4 -> setWayptAnims <$> parseInteger
  5 -> setWayptDelay <$> parseInteger
  _ -> error "impossible"
  where
  field :: Parser Int
  field = choice
        [ 0 <$ chunk "flags"
        , 1 <$ chunk "loc"
        , 2 <$ chunk "offsets"
        , 3 <$ chunk "rotation"
        , 4 <$ chunk "anims"
        , 5 <$ chunk "delay"
        ]

parseWaypointArr :: Parser WaypointArr
parseWaypointArr = finalize <$> parseJsonObject field \case
  0 -> setWayptCount <$> parseInteger
  1 -> setWayptExtra1 <$> parseInteger
  2 -> setWayptExtra2 <$> parseInteger
  3 -> setWayptExtra3 <$> parseInteger
  4 -> setWaypts <$> parseArr parseWaypoint
  _ -> error "impossible"
  where
  field :: Parser Int
  field = choice
        [ 0 <$ chunk "count"
        , 1 <$ try (chunk "extra1")
        , 2 <$ try (chunk "extra2")
        , 3 <$ try (chunk "extra3")
        , 4 <$ chunk "waypoints"
        ]

parseOffsets :: Parser Offsets
parseOffsets = finalize <$> parseJsonObject field \case
  False -> setOffX <$> parseFloat
  True -> setOffY <$> parseFloat
  where
  field = False <$ chunk "offx" <|> True <$ chunk "offy"

parseStandpoint :: Parser Standpoint
parseStandpoint = finalize <$> parseJsonObject field \case
  0 -> setStdptMap <$> parseInteger
  1 -> setStdptLoc <$> parseLoc
  2 -> setStdptOff <$> parseOffsets
  3 -> setStdptJp <$> parseInteger
  _ -> error "impossible"
  where
  field :: Parser Int
  field = choice
        [ 0 <$ chunk "map-info"
        , 1 <$ chunk "loc"
        , 2 <$ chunk "offsets"
        , 3 <$ chunk "jp"
        ]

readMob :: String -> ByteString -> Either String Mob
readMob name input = first errorBundlePretty $ runParser parseMob name input

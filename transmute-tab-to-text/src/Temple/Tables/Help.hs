
module Temple.Tables.Help
  ( HelpEntry (..)
  , validateEntry
  , encodeEntry
  , writeHelpTable
  , prettyEntries
  , parseEntryFile
  , readEntryFile
  , parseEntryLine
  , parseHelpTable
  , readHelpTable
  ) where

import Control.Applicative ((<**>))
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad (when)
import Data.Bifunctor
import Data.ByteString (ByteString, unpack)
import Data.ByteString qualified as W8
import Data.ByteString.Builder
import Data.ByteString.Char8 qualified as C8
import Data.Char (ord, chr)
import Data.Foldable
import Data.Functor
import Data.List (intersperse, isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set as S hiding (fold, foldl, filter)
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte

data HelpColumn
  = TopicTag
  | ParentTag
  | SpellListTags
  | Title
  | Body
  deriving (Eq, Ord, Show)

colName :: HelpColumn -> String
colName TopicTag = "topic tag"
colName ParentTag = "parent tag"
colName SpellListTags = "spell list tags"
colName Title = "title"
colName Body = "body"

-- Information from a help table entry
--
-- Note: the actual help table has one additional column between parent and
-- spell list entries. It appears to be an override that lets you specify
-- the topic to jump to when pressing the 'previous topic' button in the
-- UI. However, it doesn't seem to work well. Specifying it _breaks_ the
-- analogous 'next topic' button for the section, _and_ help entries are
-- automatically assembled into lists according to their parent tags and
-- the order they occur in the tables.
data HelpEntry
  = HelpEntry
  { helpTopicTag      :: ByteString   -- unique identifier for the topic
  , helpParentTag     :: ByteString   -- parent tag
  , helpSpellListTags :: [ByteString] -- spell lists to occur in
  , helpTitle         :: ByteString   -- title text
  , helpBody          :: [ByteString] -- body text, lines
  }

-- As above, but all fields are optional, to facilitate construction.
data PartialHelpEntry
  = PHelpEntry
  { pHelpTopicTag      :: Maybe ByteString
  , pHelpParentTag     :: Maybe ByteString
  , pHelpSpellListTags :: Maybe [ByteString]
  , pHelpTitle         :: Maybe ByteString
  , pHelpBody          :: Maybe [ByteString]
  }

nullEntry :: PartialHelpEntry
nullEntry = PHelpEntry Nothing Nothing Nothing Nothing Nothing

finalize :: PartialHelpEntry -> Maybe HelpEntry
finalize PHelpEntry {..} = do
  helpTopicTag <- pHelpTopicTag
  let helpParentTag = fromMaybe "" pHelpParentTag
      helpSpellListTags = fromMaybe [] pHelpSpellListTags
  helpTitle <- pHelpTitle
  helpBody <- pHelpBody
  pure HelpEntry {..}

isEmptyEntry :: HelpEntry -> Bool
isEmptyEntry (HelpEntry { .. })
  = and
  [ W8.null helpTopicTag
  , W8.null helpParentTag
  , all W8.null helpSpellListTags
  , W8.null helpTitle
  , Prelude.null helpBody
  ]

tagChars :: Set Word8
tagChars = fromList . fmap ord8 $ ['0'..'9'] ++ ['A'..'Z'] ++ "_'+"

-- Checks that a string looks like a tag.
validateTag :: ByteString -> Bool
validateTag str =
  ("TAG_" `W8.isPrefixOf` str) && W8.all (`member` tagChars) str

-- Checks that all the strings in the list look like tags.
validateTags :: [ByteString] -> Bool
validateTags = all validateTag

linebreak :: Char -> Bool
linebreak c = c == '\n' || c == '\r'

-- Checks that a HelpEntry obeys expected conventions
validateEntry :: HelpEntry -> Bool
validateEntry (HelpEntry {..})
  = and
  [ validateTag helpTopicTag
  , validateTag helpParentTag
  , validateTags helpSpellListTags
  , not $ C8.any linebreak helpTitle
  , C8.all (/='\t') helpTitle
  , all (\p -> C8.all (/='\t') p && not (C8.any linebreak p)) helpBody
  ]

intercalate :: Builder -> [Builder] -> Builder
intercalate med = fold . intersperse med

-- Renders a help entry as a line in a `help.tab` file
encodeEntry :: HelpEntry -> Builder
encodeEntry (HelpEntry {..}) =
  intercalate (char8 '\t')
    [ byteString helpTopicTag
    , byteString helpParentTag
    , byteString ""
    , encodeSpellList helpSpellListTags
    , byteString helpTitle
    , encodeBody helpBody
    ]
  where
  encodeSpellList = intercalate (char8 ' ') . fmap byteString

  encodeBody = intercalate (char8 '\x0b') . fmap byteString

writeHelpTable :: [HelpEntry] -> Builder
writeHelpTable = fold . fmap ((<> char8 '\n') . encodeEntry)

-- Renders a help entry in a human readable textual format
prettyEntry :: HelpEntry -> Builder
prettyEntry (HelpEntry {..})
  = fold
  [ byteString "{{{\n"
  , field "topic-tag" helpTopicTag
  , field "parent-tag" helpParentTag
  , fields "spell-lists" helpSpellListTags
  , field "title" helpTitle
  , byteString "|||\n"
  , prettyBody helpBody
  , byteString "\n}}}\n"
  ]
  where
  field :: ByteString -> ByteString -> Builder
  field name value
    | W8.null value = mempty
    | otherwise
    = byteString name
   <> byteString ": "
   <> byteString value
   <> byteString "\n"

  fields :: ByteString -> [ByteString] -> Builder
  fields name [] = mempty
  fields name vs
    = byteString name <> byteString ": "
   <> intercalate (char8 ' ') (byteString <$> vs)
   <> byteString "\n"

  prettyBody paras = intercalate (char8 '\n') (byteString <$> paras)

prettyEntries :: [HelpEntry] -> Builder
prettyEntries =
  intercalate (char8 '\n') . fmap prettyEntry . filter (not . isEmptyEntry)

data HelpError
  = BadColumn HelpColumn Int
  | BadEntry
  deriving (Eq, Ord, Show)

instance ShowErrorComponent HelpError where
  showErrorComponent (BadColumn col _) =
    "This " ++ colName col ++ " column looks strange to me"
  showErrorComponent BadEntry = "This whole help entry seems wrong."

  errorComponentLen (BadColumn _ len) = len
  errorComponentLen _ = 1

type HelpParser = ReaderT Bool (Parsec HelpError ByteString)

runParserPretty :: HelpParser a -> String -> Bool -> ByteString -> Either String a
runParserPretty p name strict input =
  first errorBundlePretty $ runParser (runReaderT p strict) name input

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

-- Space character parsers for separation
pspace, pspace1 :: HelpParser ()
pspace = void . many $ char (ord8 ' ')
pspace1 = void . some $ char (ord8 ' ')

-- Parses a help tag. Typically these are of the form `TAG_...` with all
-- caps. The `TAG_` prefix is not enforced here, though.
tag :: HelpParser ByteString
tag = takeWhile1P (Just "Tag character") (`member` tagChars)

eolbs :: Set Word8
eolbs = fromList $ unpack "\r\n"

-- -----------------
-- Help table parser
-- -----------------

colEnder :: Set Word8
colEnder = fromList $ unpack "\t\r\n"

-- Promotes a parser to one that must saturate a column in a table. The
-- name should be a descriptive name of the column.
--
-- The parser is run, and a lookahead is used to ensure that a column break
-- follows. If this fails, then an error recovery is performed.
--
-- In the error recovery, we check if we're in strict parsing mode. If so,
-- the error is registered for later reporting. In either case, we consume
-- input until the next column break so that we can continue analyzing the
-- file.
--
-- Note: the skip on error recovery checks for _either_ tab or linebreaks,
-- to make sure we don't 'recover' beyond the end of the line.
column :: HelpColumn -> HelpParser a -> HelpParser (Maybe a)
column col p =
  label (colName col) . withRecovery h $ Just <$> p <* lookAhead colbreak
  where
  colbreak = void tab <|> void eol

  remapError len (TrivialError off _ _) = badColumn off len col
  remapError _ err = err

  h err = Nothing <$ lookAhead eof <|> do
    strict <- ask
    rest <- takeWhileP Nothing (`notMember` colEnder)
    when strict . registerParseError $ remapError (W8.length rest) err
    pure Nothing

-- Parses a column expected to have a single tag in it.
singleTagColumn :: HelpColumn -> HelpParser (Maybe ByteString)
singleTagColumn col = column col $ pspace *> tag <* pspace

-- Parses a column that may have a single tag, but may also be blank.
optionalTagColumn :: HelpColumn -> HelpParser (Maybe ByteString)
optionalTagColumn col =
  column col $ do
    pspace
    tag <|> pure ""

-- Parses a column expected to have multiple tags separated by space.
multiTagColumn :: HelpColumn -> HelpParser (Maybe [ByteString])
multiTagColumn col = column col $ pspace *> sepEndBy tag sep
  where
  seps = fromList $ unpack ", "
  sep = takeWhile1P Nothing (`member` seps)

-- Parses a column expected to have relatively arbitrary text. Naturally,
-- that text _cannot_ include tabs or line break characters. This also
-- consumes leading and trailing space in the column.
textColumn :: HelpColumn -> HelpParser (Maybe ByteString)
textColumn col = column col $ pspace *> text <* pspace
  where
  trim = C8.dropWhileEnd (== ' ')
  text = trim <$> takeWhileP Nothing (`notMember` colEnder)

-- Parses a column that includes multi-paragraph text. This is encoded
-- specially, since it can include neither tab nor actual line break
-- characters. Each string in the result is a paragraph in the column. The
-- entire column contents are yielded, no trimming is performed, since it
-- might be necessary for getting in-game help laid out correctly.
paraColumn :: HelpColumn -> HelpParser (Maybe [ByteString])
paraColumn col = column col $ para `sepBy` string "\x0b"
  where
  -- Separator characters for paragraphs. \x0b is vertical tab, used in
  -- place of newlines in the embedded text. Others have special meaning in
  -- tab files.
  seps = fromList $ unpack "\x0b\r\n\t"

  para = takeWhileP Nothing (`notMember` seps)

-- Parses a line of a help table into a `HelpEntry`
parseEntryLine :: HelpParser PartialHelpEntry
parseEntryLine = do
  pHelpTopicTag <- singleTagColumn TopicTag
  tab
  pHelpParentTag <- optionalTagColumn ParentTag
  tab
  -- note: ignored
  label "prev tag" $ takeWhileP Nothing (/= ord8 '\t')
  tab
  pHelpSpellListTags <- multiTagColumn SpellListTags
  tab
  pHelpTitle <- textColumn Title
  tab
  pHelpBody <- paraColumn Body
  pure $ PHelpEntry {..}

-- Gets the contents of the rest of the line, not parsing the eol
line :: HelpParser ByteString
line = takeWhileP Nothing (`notMember` eolbs)

trimmedLine :: HelpParser ByteString
trimmedLine = trim <$> line where trim = C8.dropWhileEnd (== ' ')

badColumn :: Int -> Int -> HelpColumn -> ParseError s HelpError
badColumn off len col =
  FancyError off . singleton . ErrorCustom $ BadColumn col len

badEntry :: Int -> ParseError s HelpError
badEntry off = FancyError off . singleton $ ErrorCustom BadEntry

-- Parses an entire help table
parseHelpTable :: HelpParser [HelpEntry]
parseHelpTable =
  mapMaybe (finalize =<<) <$> recovered `sepEndBy` eol <* eof
  where
  recovered = withRecovery h $ Just <$> parseEntryLine

  remapError (TrivialError off _ _) = badEntry off
  remapError err = err

  h err = Nothing <$ lookAhead eof <|> do
    strict <- ask
    when strict . registerParseError $ remapError err
    Nothing <$ line -- skip to end of line

-- --------------------------
-- Human readable help parser
-- --------------------------

field :: ByteString -> HelpParser a -> HelpParser a
field name content = label (C8.unpack name) $ do
  try $ string name *> hspace *> string ":"
  hspace *> content <* hspace <* eol

singleTagField :: ByteString -> HelpParser ByteString
singleTagField name = field name tag

optionalTagField :: ByteString -> HelpParser ByteString
optionalTagField name = field name (tag <|> pure "")

multiTagField :: ByteString -> HelpParser [ByteString]
multiTagField name = field name (tag `sepBy` hspace)

textField :: ByteString -> HelpParser ByteString
textField name = field name trimmedLine

-- Parses a human readable help entry. These are of the form
--
--     {{{
--     fields
--     |||
--     body
--     }}}
--
-- `fields` is a sequence of one-line field specifications in no particular
-- order, in forms like
--
--     <field-name>: <field-content>
--
-- Some fiends have stricter expectations, like only containing one link
-- tag.
--
-- Following the vertical pipes is the body text of the help entry. The
-- only hard restriction for these are that they cannot contain a line with
-- just `}}}` on it, as that would end the help entry. However, any tabs
-- would result in an invalid corresponding help table, though they will
-- not fail to parse here.
--
-- The lines in the body are not trimmed or joined at all, because that
-- would interfere with the possibility of getting help text laid out
-- appropriately in the game UI. Other fields will be trimmed of whitespace
-- somewhat.
parseTextEntry :: HelpParser PartialHelpEntry
parseTextEntry = applied <$> (opener *> fields <**> body)
  where
  applied = foldl (\e f -> f e) nullEntry
  opener = void $ string "{{{" *> hspace *> eol
  bodyBegin = void $ string "|||" *> hspace *> eol
  closer = void $ string "}}}" *> hspace *> eol

  fields = flip manyTill bodyBegin $
    choice
      [ updateTopic <$> singleTagField "topic-tag"
      , updateParent <$> optionalTagField "parent-tag"
      , updateSpells <$> multiTagField "spell-lists"
      , updateTitle <$> textField "title"
      , id <$ hspace <* eol -- empty line
      ]

  updateTopic tag entry = entry { pHelpTopicTag = Just tag }
  updateParent tag entry = entry { pHelpParentTag = Just tag }
  updateSpells tags entry = entry { pHelpSpellListTags = Just tags }
  updateTitle text entry = entry { pHelpTitle = Just text }
  updateBody paras entry = entry { pHelpBody = Just paras }

  body = (:) . updateBody <$> manyTill (line <* eol) closer

parseEntryFile :: HelpParser [HelpEntry]
parseEntryFile =
  mapMaybe finalize <$> sepEndBy parseTextEntry (many $ hspace <* eol) <* eof

formatTableError ::
  Maybe String -> SourcePos -> ParseError ByteString HelpError -> String
formatTableError mline spos err
  = ppos -- file location
  . nl
  . dispLine
  . nl
  . showString (parseErrorTextPretty err)
  . nl
  $ ""
  where
  nl = showString "\n"
  ppos = showString $ sourcePosPretty spos

  dispLine = case mline of
    Nothing -> id
    Just line -> case slice line . unPos $ sourceColumn spos of
      (snip, pad) ->
        showString snip . nl . showString (replicate (pad-1) ' ') . ('^':)

  slice line col
    | length line < 80 = (line, col)
  slice line col
    | col > 40 = ("..." ++ Prelude.take 74 (Prelude.drop (col - 34) line) ++ "...", 37)
    | otherwise = (Prelude.take 80 line, col)

readEntryFile :: Bool -> String -> ByteString -> Either String [HelpEntry]
readEntryFile strict name input
  = first errorBundlePretty
  $ runParser (runReaderT parseEntryFile strict) name input

readHelpTable :: Bool -> String -> ByteString -> Either String [HelpEntry]
readHelpTable strict name input
  = first (errorBundlePrettyWith formatTableError)
  $ runParser (runReaderT parseHelpTable strict) name input

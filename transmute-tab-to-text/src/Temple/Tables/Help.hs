
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
import Data.Bifunctor
import Data.ByteString (ByteString, unpack)
import Data.ByteString qualified as W8
import Data.ByteString.Builder
import Data.ByteString.Char8 qualified as C8
import Data.Char (ord, chr)
import Data.Foldable
import Data.Functor
import Data.List (intersperse, isPrefixOf)
import Data.Set as S hiding (fold, foldl, filter)
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte

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

emptyEntry :: HelpEntry
emptyEntry = HelpEntry "" "" [] "" []

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
writeHelpTable = intercalate (char8 '\n') . fmap encodeEntry

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

type Parser = Parsec Void ByteString

runParserPretty :: Parser a -> String -> ByteString -> Either String a
runParserPretty p name input =
  first errorBundlePretty $ runParser p name input

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

-- Space character parsers for separation
pspace, pspace1 :: Parser ()
pspace = void . many $ char (ord8 ' ')
pspace1 = void . some $ char (ord8 ' ')

-- Parses a help tag. Typically these are of the form `TAG_...` with all
-- caps. The `TAG_` prefix is not enforced here, though.
tag :: Parser ByteString
tag = takeWhile1P (Just "Tag character") (`member` tagChars)

eolbs :: Set Word8
eolbs = fromList $ unpack "\r\n"

-- -----------------
-- Help table parser
-- -----------------

-- Parses a column expected to have a single tag in it.
singleTagColumn :: Parser ByteString
singleTagColumn = pspace *> tag <* pspace

-- Parses a column that may have a single tag, but may also be blank.
optionalTagColumn :: Parser ByteString
optionalTagColumn = pspace *> (tag <|> pure "") <* pspace

-- Parses a column expected to have multiple tags separated by space.
multiTagColumn :: Parser [ByteString]
multiTagColumn = pspace *> sepEndBy tag sep
  where
  seps = fromList $ unpack ", "
  sep = takeWhile1P Nothing (`member` seps)

-- Parses a column expected to have relatively arbitrary text. Naturally,
-- that text _cannot_ include tabs or line break characters. This also
-- consumes leading and trailing space in the column.
textColumn :: Parser ByteString
textColumn = pspace *> text <* pspace
  where
  ender = fromList $ unpack "\t\r\n"
  trim = C8.dropWhileEnd (== ' ')
  text = trim <$> takeWhileP Nothing (`notMember` ender)

-- Parses a column that includes multi-paragraph text. This is encoded
-- specially, since it can include neither tab nor actual line break
-- characters. Each string in the result is a paragraph in the column. The
-- entire column contents are yielded, no trimming is performed, since it
-- might be necessary for getting in-game help laid out correctly.
paraColumn :: Parser [ByteString]
paraColumn = para `sepBy` string "\x0b"
  where
  -- Separator characters for paragraphs. \x0b is vertical tab, used in
  -- place of newlines in the embedded text. Others have special meaning in
  -- tab files.
  seps = fromList $ unpack "\x0b\r\n\t"

  para = takeWhileP Nothing (`notMember` seps)

-- Parses a line of a help table into a `HelpEntry`
parseEntryLine :: Parser HelpEntry
parseEntryLine = do
  helpTopicTag <- label "entry tag" singleTagColumn
  tab
  helpParentTag <- label "parent tag" optionalTagColumn
  tab
  -- note: ignored
  label "prev tag" $ takeWhileP Nothing (/= ord8 '\t')
  tab
  helpSpellListTags <- label "spell lists" multiTagColumn
  tab
  helpTitle <- label "title" textColumn
  tab
  helpBody <- label "body" paraColumn
  pure $ HelpEntry {..}

-- Gets the contents of the rest of the line, not parsing the eol
line :: Parser ByteString
line = takeWhileP Nothing (`notMember` eolbs)

trimmedLine :: Parser ByteString
trimmedLine = trim <$> line where trim = C8.dropWhileEnd (== ' ')

skipRestOfLine :: a -> b -> Parser a
skipRestOfLine x _ = x <$ line

-- Parses an entire help table
parseHelpTable :: Parser [HelpEntry]
parseHelpTable = recovered `sepEndBy` eol <* eof
  where
  recovered = withRecovery (skipRestOfLine emptyEntry) parseEntryLine

-- --------------------------
-- Human readable help parser
-- --------------------------

field :: ByteString -> Parser a -> Parser a
field name content = label (C8.unpack name) $ do
  try $ string name *> hspace *> string ":"
  hspace *> content <* hspace <* eol

singleTagField :: ByteString -> Parser ByteString
singleTagField name = field name tag

optionalTagField :: ByteString -> Parser ByteString
optionalTagField name = field name (tag <|> pure "")

multiTagField :: ByteString -> Parser [ByteString]
multiTagField name = field name (tag `sepBy` hspace)

textField :: ByteString -> Parser ByteString
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
parseTextEntry :: Parser HelpEntry
parseTextEntry = applied <$> (opener *> fields <**> body)
  where
  applied = foldl (\e f -> f e) emptyEntry
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

  updateTopic tag entry = entry { helpTopicTag = tag }
  updateParent tag entry = entry { helpParentTag = tag }
  updateSpells tags entry = entry { helpSpellListTags = tags }
  updateTitle text entry = entry { helpTitle = text }
  updateBody paras entry = entry { helpBody = paras }

  body = (:) . updateBody <$> manyTill (line <* eol) closer

parseEntryFile :: Parser [HelpEntry]
parseEntryFile = sepEndBy parseTextEntry (many $ hspace <* eol) <* eof

readEntryFile :: String -> ByteString -> Either String [HelpEntry]
readEntryFile name input =
  first errorBundlePretty $ runParser parseEntryFile name input

readHelpTable :: String -> ByteString -> Either String [HelpEntry]
readHelpTable name input =
  first errorBundlePretty $ runParser parseHelpTable name input

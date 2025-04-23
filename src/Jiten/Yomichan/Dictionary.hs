{-# LANGUAGE RecordWildCards #-}

module Jiten.Yomichan.Dictionary where

import qualified Codec.Archive.Zip as Zip
import Conduit (ConduitT, (.|))
import qualified Conduit
import Control.Applicative ((<|>))
import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON, Value, parseJSON, (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, prependFailure)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Aeson (conduitArray)
import qualified Data.Conduit.Combinators as Conduit.Combinators
import Data.Either.Extra (maybeToEither)
import Data.Function ((&))
import qualified Data.List as List
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Jiten.Util as Util
import System.FilePath (takeExtension)

-- DICTIONARY INDEX ------------------------------------------------------------

data UpdateInfo = UpdateInfo
  { -- | URL for the index file of the latest revision of the dictionary,
    -- used to check for updates.
    updateInfoIndexUrl :: !Text,
    -- | URL for the download of the latest revision of the dictionary.
    updateInfoDownloadUrl :: !Text
  }
  deriving (Show)

data Index = Index
  { -- | Title of the dictionary.
    indexTitle :: !Text,
    -- | Revision of the dictionary.
    -- This value is displayed, and used to check for dictionary updates.
    indexRevision :: !Text,
    -- | Whether or not this dictionary contains sequencing information for related terms.
    indexSequenced :: !Bool,
    -- | Format of data found in the JSON data files.
    indexFormat :: !Int,
    -- | Creator of the dictionary.
    indexAuthor :: !(Maybe Text),
    -- | Whether this dictionary contains links to its latest version.
    indexIsUpdatable :: !(Maybe UpdateInfo),
    -- | URL for the source of the dictionary, displayed in the dictionary details.
    indexUrl :: !(Maybe Text),
    -- | Description of the dictionary data.
    indexDescription :: !(Maybe Text),
    -- | Attribution information for the dictionary data.
    indexAttribution :: !(Maybe Text),
    -- | Language of the terms in the dictionary.
    indexSourceLanguage :: !(Maybe Text),
    -- | Main language of the definitions in the dictionary.
    indexTargetLanguage :: !(Maybe Text),
    indexFrequencyMode :: !(Maybe Text)
  }
  deriving (Show)

instance FromJSON Index where
  parseJSON = A.withObject "dictionaryIndex" $ \obj -> do
    sequenced <- do
      sequencedOpt <- obj .:? "sequenced" :: Parser (Maybe Bool)
      pure (isJust sequencedOpt)
    isUpdatable <- do
      isUpdatableOpt <- obj .:? "isUpdatable" :: Parser (Maybe Bool)
      case isUpdatableOpt of
        Nothing -> pure Nothing
        Just _ ->
          Just <$> (UpdateInfo <$> obj .: "indexUrl" <*> obj .: "downloadUrl")
    format <- do
      formatOpt <- obj .:? "format" <|> obj .:? "version"
      case formatOpt of
        Nothing -> fail "index must contain either 'format' or 'version' field"
        Just f -> pure f
    Index
      <$> obj .: "title"
      <*> obj .: "revision"
      <*> pure sequenced
      <*> pure format
      <*> obj .:? "author"
      <*> pure isUpdatable
      <*> obj .:? "url"
      <*> obj .:? "description"
      <*> obj .:? "attribution"
      <*> obj .:? "sourceLanguage"
      <*> obj .:? "targetLanguage"
      <*> obj .:? "frequencyMode"

-- KANJI -----------------------------------------------------------------------

-- | Information about a single kanji character.
data Kanji = Kanji
  { -- | Kanji character.
    kanjiCharacter :: !Text,
    -- | Onyomi readings for the kanji character.
    kanjiOnyomi :: ![Text],
    -- | Kunyomi readings for the kanji character.
    kanjiKunyomi :: ![Text],
    -- | Tags for the kanji character.
    kanjiTags :: ![Text],
    -- | Meanings for the kanji character.
    kanjiMeanings :: ![Text],
    -- | Various stats for the kanji character.
    kanjiStats :: ![(Text, Text)]
  }
  deriving (Show)

instance FromJSON Kanji where
  parseJSON v =
    parseJSON v >>= \case
      [kj, oy, ky, ts, ms, ss] ->
        Kanji
          <$> parseJSON kj
          <*> fmap (Text.splitOn " ") (parseJSON oy)
          <*> fmap (Text.splitOn " ") (parseJSON ky)
          <*> fmap (Text.splitOn " ") (parseJSON ts)
          <*> parseJSON ms
          <*> parseStats ss
      _other -> fail "invalid Kanji"
    where
      parseStats (A.Object obj) = do
        let keyMap = KeyMap.toList obj
            txtKeyMap = fmap (first Key.toText) keyMap
        mapM (traverse parseJSON) txtKeyMap
      parseStats _ = fail "invalid kanji stat"

-- KANJI META ------------------------------------------------------------------

-- | Metadata about a single kanji character.
data KanjiMeta = KanjiMeta
  { kanjiMetaKanji :: !Text,
    -- | Data for the character.
    kanjiMetaData :: !Value
  }
  deriving (Show)

instance FromJSON KanjiMeta where
  parseJSON v =
    parseJSON v >>= \case
      [kj, _, fq] -> KanjiMeta <$> parseJSON kj <*> pure fq
      _other -> fail "invalid KanjiMeta"

-- TAG -------------------------------------------------------------------------

data Tag = Tag
  { -- | Tag name.
    tagName :: !Text,
    -- | Category for the tag.
    tagCategory :: !Text,
    -- | Sorting order for the tag.
    tagSortingOrder :: !Int,
    -- | Notes for the tag.
    tagNotes :: !Text,
    -- | Score used to determine popularity.
    -- Negative values are more rare and positive values are more frequent.
    -- This score is also used to sort search results.
    tagScore :: !Int
  }
  deriving (Show)

instance FromJSON Tag where
  parseJSON v =
    parseJSON v >>= \case
      [nm, c, o, no, s] ->
        Tag
          <$> parseJSON nm
          <*> parseJSON c
          <*> parseJSON o
          <*> parseJSON no
          <*> parseJSON s
      _other -> fail "invalid Tag"

-- TERM ------------------------------------------------------------------------

data Term = Term
  { -- | The text for the term.
    termText :: !Text,
    -- | Reading of the term, or Nothing if the reading is the same as the term.
    termReading :: !(Maybe Text),
    -- | Tags for the definition.
    termDefinitionTags :: !Text,
    -- | Rule identifiers for the definition which is used to validate deinflection.
    -- An empty list should be used for words which aren't inflected.
    termRuleIdentifiers :: !Text,
    -- | Score used to determine popularity.
    -- Negative values are more rare and positive values are more frequent.
    -- This score is also used to sort search results.
    termPopularity :: !Int,
    -- | List of definitions for the term.
    termDefinitions :: !Value,
    -- | Sequence number for the term. Terms with the same sequence number can be shown together.
    termSequenceNumber :: !Int,
    -- | Tags for the term.
    termTermTags :: !Text
  }
  deriving (Show)

instance FromJSON Term where
  parseJSON v =
    parseJSON v >>= \case
      [tm, rd, dt, ri, p, d, sn, tt] ->
        Term
          <$> field "term" (parseJSON tm)
          <*> field "reading" (parseReading rd)
          <*> field "definitionTags" (parseDefinitionTags dt)
          <*> field "ruleIdentifiers" (parseJSON ri)
          <*> field "popularity" (parseJSON p)
          <*> pure d -- field "definitions" (parseJSON d)
          <*> field "sequenceNumber" (parseJSON sn)
          <*> field "termTags" (parseJSON tt)
      _other -> fail "invalid Term"
    where
      field f = prependFailure ("parsing Term field " <> f <> " failed, ")
      parseReading rd =
        fmap (\tx -> if Text.null tx then Nothing else Just tx) (parseJSON rd)
      parseDefinitionTags :: Value -> Parser Text
      parseDefinitionTags (A.String s) = pure s
      parseDefinitionTags _ = pure ""

-- TERM META -------------------------------------------------------------------

-- | Metadata about a single term.
data TermMeta = TermMeta
  { termMetaTerm :: !Text,
    termMetaMode :: !Text,
    termMetaData :: !Value
  }
  deriving (Show)

instance FromJSON TermMeta where
  parseJSON v = do
    l <- parseJSON v
    case l of
      [tm, ty, dt] -> TermMeta <$> parseJSON tm <*> parseJSON ty <*> pure dt
      _other -> fail "invalid TermMeta structure"

ppTermMeta :: TermMeta -> Text
ppTermMeta (TermMeta {..}) =
  Util.encodeStrictText
    ( A.Array
        ( Vector.fromList
            [A.String termMetaTerm, A.String termMetaMode, termMetaData]
        )
    )

-- ARCHIVES --------------------------------------------------------------------

newtype DictionaryImportException = DictionaryImportException Text
  deriving (Show)

instance Exception DictionaryImportException

throwImport :: Text -> IO a
throwImport = throwIO . DictionaryImportException

data Dictionary = Dictionary
  { dictionaryIndex :: !Index,
    dictionaryArchive :: !Zip.Archive
  }

openArchiveFile :: FilePath -> IO Dictionary
openArchiveFile fp = do
  archive <- LBS.readFile fp
  case openArchive archive of
    Right dict -> pure dict
    Left err -> throwImport err

openArchive :: LBS.ByteString -> Either Text Dictionary
openArchive bs = do
  archive <- Zip.toArchiveOrFail bs & first Text.pack
  indexEntry <-
    Zip.findEntryByPath "index.json" archive & maybeToEither "missing index.json"
  index <- A.eitherDecode (Zip.fromEntry indexEntry) & first Text.pack
  if indexFormat index == 3
    then pure (Dictionary index archive)
    else Left "format not supported"

type C a = ConduitT () a IO ()

streamBanks :: (FromJSON a) => String -> Dictionary -> C a
streamBanks prefix dict =
  let archive = dictionaryArchive dict
      files = Zip.filesInArchive archive
      bankFiles = filter (List.isPrefixOf prefix) files
      streams =
        bankFiles
          & map
            ( \fp ->
                case Zip.findEntryByPath fp archive of
                  Nothing -> Conduit.yieldM (throwImport ("failed to open " <> Text.pack fp))
                  Just entry ->
                    Conduit.yield (LBS.toStrict (Zip.fromEntry entry))
                      .| conduitArray
            )
   in sequence_ streams

streamTerms :: Dictionary -> C Term
streamTerms = streamBanks "term_bank"

streamTermMetas :: Dictionary -> C TermMeta
streamTermMetas = streamBanks "term_meta_bank"

streamTags :: Dictionary -> C Tag
streamTags = streamBanks "tag_bank"

streamKanji :: Dictionary -> C Kanji
streamKanji = streamBanks "kanji_bank"

streamKanjiMetas :: Dictionary -> C KanjiMeta
streamKanjiMetas = streamBanks "kanji_meta_bank"

runStream :: C a -> (a -> IO ()) -> IO ()
runStream stream f = Conduit.runConduit (stream .| Conduit.Combinators.mapM_ f)

getMedia :: Dictionary -> Either Text [(FilePath, LBS.ByteString)]
getMedia dict =
  let archive = dictionaryArchive dict
      files = Zip.filesInArchive archive
      mediaFiles =
        filter (not . List.isSuffixOf "/")
          . filter ((/= ".json") . takeExtension)
          $ files
   in mediaFiles
        & traverse
          ( \fp ->
              let entryMay = Zip.findEntryByPath fp archive
               in case entryMay of
                    Nothing -> Left "failed to read media file"
                    Just entry -> Right (fp, Zip.fromEntry entry)
          )

{-# LANGUAGE RecordWildCards #-}

module Jiten.Database where

import Control.Monad (forM_, void)
import qualified Control.Monad as Monad
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson.Text as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (getCurrentTime)
import Database.SQLite.Simple (Connection, FromRow (..), Only (Only), Query (..), ToRow (..), execute, execute_, field, lastInsertRowId, query, query_, withTransaction)
import Formatting (sformat, (%))
import qualified Formatting
import Jiten.Yomichan (throwImport)
import qualified Jiten.Yomichan as Yomichan

type DictionaryId = Int64

data HeadingRow = HeadingRow {headingTerm :: Text, headingReading :: Maybe Text}

data KanjiRow = KanjiRow
  { kanjiCharacter :: !Text,
    kanjiOnyomi :: ![Text],
    kanjiKunyomi :: ![Text],
    kanjiTags :: ![Text],
    kanjiMeanings :: ![Text],
    kanjiStats :: ![(Text, Text)],
    kanjiDictionaryId :: Int64
  }

instance FromRow HeadingRow where
  fromRow = HeadingRow <$> field <*> field

instance ToRow KanjiRow where
  toRow (KanjiRow {..}) =
    toRow
      ( kanjiCharacter,
        T.intercalate "," kanjiOnyomi,
        T.intercalate "," kanjiKunyomi,
        T.intercalate "," kanjiTags,
        T.intercalate "," kanjiMeanings,
        LT.toStrict . A.encodeToLazyText $ kanjiStats,
        kanjiDictionaryId
      )

-- TODO: should we include termRuleIdentifiers?
initDatabase :: Connection -> IO ()
initDatabase conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS heading(id INTEGER PRIMARY KEY,term VARCHAR NOT NULL,reading VARCHAR NULL,CONSTRAINT unique_heading UNIQUE (term,reading))"
  execute_ conn "CREATE TABLE IF NOT EXISTS dictionary(id INTEGER PRIMARY KEY,name VARCHAR NOT NULL,revision VARCHAR NOT NULL,sequenced BOOLEAN NOT NULL,import_date TIMESTAMP NOT NULL,CONSTRAINT unique_dict_name UNIQUE (name))"
  execute_ conn "CREATE TABLE IF NOT EXISTS entry(id INTEGER PRIMARY KEY,glossary VARCHAR NOT NULL,definition_tags VARCHAR NOT NULL,term_tags VARCHAR NOT NULL,popularity INTEGER NOT NULL,rules VARCHAR NOT NULL,heading_id INTEGER NOT NULL REFERENCES heading ON DELETE RESTRICT ON UPDATE RESTRICT,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS tag(id INTEGER PRIMARY KEY,name VARCHAR NOT NULL,category VARCHAR NOT NULL,sorting_order INTEGER NOT NULL,notes VARCHAR NOT NULL,popularity INTEGER NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_tag_name UNIQUE (name,dictionary_id))"
  execute_ conn "CREATE TABLE IF NOT EXISTS kanji(id INTEGER PRIMARY KEY,kanji VARCHAR NOT NULL,onyomi VARCHAR NOT NULL,kunyomi VARCHAR NOT NULL,tags VARCHAR NOT NULL,meanings VARCHAR NOT NULL,stats BLOB NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_kanji UNIQUE (kanji,dictionary_id))"
  execute_ conn "CREATE TABLE IF NOT EXISTS kanji_frequency(id INTEGER PRIMARY KEY,kanji VARCHAR NOT NULL,data VARCHAR NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_kanji_frequency UNIQUE (kanji,data,dictionary_id))"
  execute_ conn "CREATE TABLE IF NOT EXISTS media(id INTEGER PRIMARY KEY,path VARCHAR NOT NULL,content BLOB NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_path UNIQUE (path,dictionary_id))"
  execute_ conn "CREATE TABLE IF NOT EXISTS term_meta(id INTEGER PRIMARY KEY,term VARCHAR NOT NULL,mode VARCHAR NOT NULL,data VARCHAR NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_term_meta UNIQUE (term,mode,data,dictionary_id))"
  execute_ conn "CREATE INDEX IF NOT EXISTS heading_reading_ix ON heading(reading)"
  execute_ conn "CREATE INDEX IF NOT EXISTS entry_heading_ix ON entry(heading_id)"

insertIndex :: Connection -> Yomichan.Index -> IO Int64
insertIndex conn index = do
  now <- getCurrentTime
  let dictRow = (name, revision, sequenced, now)
  execute conn "INSERT INTO dictionary (name, revision, sequenced, import_date) VALUES (?,?,?,?)" dictRow
  lastInsertRowId conn
  where
    name = Yomichan.indexTitle index
    revision = Yomichan.indexRevision index
    sequenced = Yomichan.indexSequenced index

findDictionaryId :: Connection -> Text -> IO (Maybe DictionaryId)
findDictionaryId conn dictName = do
  query conn "SELECT id FROM dictionary WHERE name = ?" (Only dictName) >>= \case
    (Only dictId : _) -> pure (Just dictId)
    [] -> pure Nothing

getDictionaries :: Connection -> IO [(DictionaryId, Text)]
getDictionaries conn = query_ conn "SELECT id, name FROM dictionary"

getOrInsertHeading :: Connection -> Text -> Maybe Text -> IO Int64
getOrInsertHeading conn term reading = do
  query conn "SELECT id FROM heading WHERE term = ? AND reading = ?" (term, reading) >>= \case
    (Only hId : _) -> pure hId
    [] -> do
      execute conn "INSERT INTO heading (term, reading) VALUES (?,?)" (term, reading)
      lastInsertRowId conn

insertTag :: Connection -> DictionaryId -> Yomichan.Tag -> IO Int64
insertTag conn dictId tag = do
  execute
    conn
    "INSERT INTO tag (name, category, sorting_order, notes, popularity, dictionary_id) VALUES (?,?,?,?,?,?)"
    ( Yomichan.tagName tag,
      Yomichan.tagCategory tag,
      Yomichan.tagSortingOrder tag,
      Yomichan.tagNotes tag,
      Yomichan.tagScore tag,
      dictId
    )
  lastInsertRowId conn

insertTerm :: Connection -> DictionaryId -> Yomichan.Term -> IO Int64
insertTerm conn dictId term = do
  hId <-
    getOrInsertHeading
      conn
      (Yomichan.termText term)
      (Yomichan.termReading term)
  execute
    conn
    "INSERT INTO entry (glossary, definition_tags, term_tags, popularity, rules, heading_id, dictionary_id) VALUES (?,?,?,?,?,?,?)"
    ( LT.toStrict . A.encodeToLazyText $ Yomichan.termDefinitions term,
      Yomichan.termDefinitionTags term,
      Yomichan.termTermTags term,
      Yomichan.termPopularity term,
      Yomichan.termRuleIdentifiers term,
      hId,
      dictId
    )
  lastInsertRowId conn

insertTermMeta :: Connection -> DictionaryId -> Yomichan.TermMeta -> IO ()
insertTermMeta conn dictId termMeta = do
  execute
    conn
    "INSERT INTO term_meta (term, mode, data, dictionary_id) VALUES (?,?,?,?)"
    ( Yomichan.termMetaTerm termMeta,
      Yomichan.termMetaMode termMeta,
      LT.toStrict . A.encodeToLazyText $ Yomichan.termMetaData termMeta,
      dictId
    )

insertKanji :: Connection -> DictionaryId -> Yomichan.Kanji -> IO ()
insertKanji conn dictId kanji =
  execute
    conn
    "INSERT INTO kanji (kanji, onyomi, kunyomi, tags, meanings, stats, dictionary_id) VALUES (?,?,?,?,?,?,?)"
    ( KanjiRow
        (Yomichan.kanjiCharacter kanji)
        (Yomichan.kanjiOnyomi kanji)
        (Yomichan.kanjiKunyomi kanji)
        (Yomichan.kanjiTags kanji)
        (Yomichan.kanjiMeanings kanji)
        (Yomichan.kanjiStats kanji)
        dictId
    )

insertKanjiMeta :: Connection -> DictionaryId -> Yomichan.KanjiMeta -> IO ()
insertKanjiMeta conn dictId kanjiMeta =
  execute
    conn
    "INSERT INTO kanji_frequency (kanji, data, dictionary_id) VALUES (?,?,?)"
    ( Yomichan.kanjiMetaKanji kanjiMeta,
      LT.toStrict . A.encodeToLazyText $ Yomichan.kanjiMetaData kanjiMeta,
      dictId
    )

insertMedia :: Connection -> DictionaryId -> FilePath -> ByteString -> IO ()
insertMedia conn dictId path content =
  execute
    conn
    "INSERT INTO media (path, content, dictionary_id) VALUES (?,?,?)"
    (path, content, dictId)

insertDictionary :: Connection -> Yomichan.Dictionary -> IO ()
insertDictionary conn dict = do
  let index = Yomichan.dictionaryIndex dict
  dictIdMay <- liftIO $ findDictionaryId conn (Yomichan.indexTitle index)
  case dictIdMay of
    Nothing -> withTransaction conn $ do
      dictId <- insertIndex conn index
      let runStream s f = Yomichan.runStream (s dict) (void . f conn dictId)
      runStream Yomichan.streamTerms insertTerm
      runStream Yomichan.streamTermMetas insertTermMeta
      runStream Yomichan.streamTags insertTag
      runStream Yomichan.streamKanji insertKanji
      runStream Yomichan.streamKanjiMetas insertKanjiMeta
      case Yomichan.getMedia dict of
        Left err -> throwImport err
        Right media ->
          forM_
            media
            ( \(fp, content) ->
                insertMedia conn dictId fp (LBS.toStrict content)
            )
    Just _ -> throwImport "dictionary is already imported"

-- SEARCH ----------------------------------------------------------------------

data TermResult = TermResult
  { termResultEntryId :: !Int64,
    termResultExpression :: !Text,
    termResultReading :: !Text,
    termResultMatchSource :: !Text,
    termResultGlossary :: !Text,
    termResultDefinitionTags :: !Text,
    termResultTermTags :: !Text,
    termResultScore :: !Int,
    termResultDictionary :: !Text,
    termResultIndex :: !Int
  }
  deriving (Show, Eq)

findTermsBulk :: Connection -> [Text] -> [DictionaryId] -> IO [TermResult]
findTermsBulk _ _ [] = pure []
findTermsBulk conn texts dictIds = do
  rows <- concat <$> Monad.forM texts findTerm
  pure $ zipWith (\i (text, row) -> mkResult i text row) [0 ..] rows
  where
    findTerm text =
      let sqlQuery =
            mconcat
              [ "SELECT heading.term, heading.reading, entry.id,glossary,definition_tags,term_tags,popularity,dictionary.name",
                " FROM heading INNER JOIN entry ON heading.id = entry.heading_id",
                " INNER JOIN dictionary ON dictionary.id = entry.dictionary_id",
                " WHERE ",
                "dictionary.id IN (",
                foldr1 (\x y -> x <> "," <> y) (fmap (pack . show) dictIds),
                ")",
                " AND (heading.term = ? OR heading.reading = ?)"
              ]
       in do
            rows <- query conn (Query sqlQuery) (text, text)
            pure (map (text,) rows)

    mkResult i text (term, reading, entryId, glossary, defTags, termTags, score, dictionary) =
      let src = if term == text then "term" else "reading"
       in TermResult entryId term reading src glossary defTags termTags score dictionary i

termResultToJSON :: TermResult -> Text
termResultToJSON (TermResult {..}) =
  mconcat
    [ "{",
      sformat ("\"entryId\": " % Formatting.int % ", ") termResultEntryId,
      sformat ("\"expression\": \"" % Formatting.stext % "\", ") termResultExpression,
      sformat ("\"reading\": \"" % Formatting.stext % "\", ") termResultReading,
      sformat ("\"matchSource\": \"" % Formatting.stext % "\", ") termResultMatchSource,
      sformat ("\"glossary\": " % Formatting.stext % ", ") termResultGlossary,
      sformat ("\"definitionTags\": \"" % Formatting.stext % "\", ") termResultDefinitionTags,
      sformat ("\"termTags\": \"" % Formatting.stext % "\", ") termResultTermTags,
      sformat ("\"score\": " % Formatting.int % ", ") termResultScore,
      sformat ("\"dictionary\": \"" % Formatting.stext % "\", ") termResultDictionary,
      sformat ("\"index\": " % Formatting.int) termResultIndex,
      "}"
    ]

data TermMetaResult = TermMetaResult
  { termMetaResultIndex :: !Int,
    termMetaResultTerm :: !Text,
    termMetaResultMode :: !Text,
    termMetaResultData :: !Text,
    termMetaResultDictionary :: !Text
  }
  deriving (Show, Eq)

findTermMetaBulk :: Connection -> [Text] -> [DictionaryId] -> IO [TermMetaResult]
findTermMetaBulk _ _ [] = pure []
findTermMetaBulk conn texts dictIds = do
  rows <- concat <$> Monad.forM texts findTermMeta
  pure $ zipWith mkResult [0 ..] rows
  where
    findTermMeta text =
      let sqlQuery =
            mconcat
              [ "SELECT term, mode, data, dictionary.name",
                " FROM term_meta INNER JOIN dictionary ON dictionary.id = term_meta.dictionary_id",
                " WHERE ",
                "dictionary.id IN (",
                foldr1 (\x y -> x <> "," <> y) (fmap (pack . show) dictIds),
                ")",
                " AND (term_meta.term = ?)"
              ]
       in query conn (Query sqlQuery) (Only text)
    mkResult i (term, mode, data_, dictionary) = TermMetaResult i term mode data_ dictionary

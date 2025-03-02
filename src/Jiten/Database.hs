{-# LANGUAGE RecordWildCards #-}

module Jiten.Database where

import Control.Monad (forM_, void)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson.Text as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (getCurrentTime)
import Database.SQLite.Simple (Connection, FromRow (..), Only (Only), ToRow (..), execute, execute_, field, lastInsertRowId, query)
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
        T.intercalate "," $ kanjiOnyomi,
        T.intercalate "," $ kanjiKunyomi,
        T.intercalate "," $ kanjiTags,
        T.intercalate "," $ kanjiMeanings,
        LT.toStrict . A.encodeToLazyText $ kanjiStats,
        kanjiDictionaryId
      )

initDatabase :: Connection -> IO ()
initDatabase conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS heading(id INTEGER PRIMARY KEY,term VARCHAR NOT NULL,reading VARCHAR NULL,CONSTRAINT unique_heading UNIQUE (term,reading))"
  execute_ conn "CREATE TABLE IF NOT EXISTS dictionary(id INTEGER PRIMARY KEY,name VARCHAR NOT NULL,revision VARCHAR NOT NULL,sequenced BOOLEAN NOT NULL,import_date TIMESTAMP NOT NULL,CONSTRAINT unique_dict_name UNIQUE (name))"
  execute_ conn "CREATE TABLE IF NOT EXISTS entry(id INTEGER PRIMARY KEY,glossary VARCHAR NOT NULL,definition_tags VARCHAR NOT NULL,term_tags VARCHAR NOT NULL,popularity INTEGER NOT NULL,rules VARCHAR NOT NULL,heading_id INTEGER NOT NULL REFERENCES heading ON DELETE RESTRICT ON UPDATE RESTRICT,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS tag(id INTEGER PRIMARY KEY,name VARCHAR NOT NULL,category VARCHAR NOT NULL,sorting_order INTEGER NOT NULL,notes VARCHAR NOT NULL,popularity INTEGER NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_tag_name UNIQUE (name,dictionary_id))"
  execute_ conn "CREATE TABLE IF NOT EXISTS kanji(id INTEGER PRIMARY KEY,kanji VARCHAR NOT NULL,onyomi VARCHAR NOT NULL,kunyomi VARCHAR NOT NULL,tags VARCHAR NOT NULL,meanings VARCHAR NOT NULL,stats BLOB NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_kanji UNIQUE (kanji,dictionary_id))"
  execute_ conn "CREATE TABLE IF NOT EXISTS kanji_frequency(id INTEGER PRIMARY KEY,kanji VARCHAR NOT NULL,data VARCHAR NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_kanji_frequency UNIQUE (kanji,dictionary_id))"
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
    "INSERT INTO kanji (kanji, onyomi, kunyomi, tags, meanings, stats) VALUES (?,?,?,?,?,?)"
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

insertDictionary :: Connection -> Yomichan.Dictionary -> ExceptT Text IO ()
insertDictionary conn dict = do
  let index = Yomichan.dictionaryIndex dict
  dictIdMay <- liftIO $ findDictionaryId conn (Yomichan.indexTitle index)
  case dictIdMay of
    Nothing -> do
      dictId <- liftIO $ insertIndex conn index
      let runStream s f = Yomichan.runStream (s dict) (liftIO . void . f conn dictId)
      runStream Yomichan.streamTerms insertTerm
      runStream Yomichan.streamTermMetas insertTermMeta
      runStream Yomichan.streamTags insertTag
      runStream Yomichan.streamKanji insertKanji
      runStream Yomichan.streamKanjiMetas insertKanjiMeta
      forM_ (Yomichan.getMedia dict) $ \(fp, content) ->
        liftIO (insertMedia conn dictId fp (LBS.toStrict content))
    Just _ -> throwError "dictionary is already imported"

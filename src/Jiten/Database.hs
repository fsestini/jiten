{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Jiten.Database where

import Control.Monad (forM_, void)
import qualified Control.Monad as Monad
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, ToJSON (..), (.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import Data.Aeson.Types (FromJSON (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import Data.Int (Int64)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (getCurrentTime)
import Database.SQLite.Simple (Connection, FromRow (..), Only (Only), Query (..), ToRow (..), execute, execute_, field, lastInsertRowId, query, query_, withTransaction)
import Formatting (sformat, (%))
import qualified Formatting
import qualified Jiten.Util as Util
import qualified Jiten.Yomichan.Dictionary as Yomichan
import qualified Jiten.Yomichan.Summary as Yomichan

class ToTextJSON a where
  toTextJSON :: a -> Text

newtype AesonValue a = AesonValue a

instance (ToJSON a) => ToTextJSON (AesonValue a) where
  toTextJSON (AesonValue v) = LT.toStrict (A.encodeToLazyText v)

instance (ToTextJSON a) => ToTextJSON [a] where
  toTextJSON v = "[" <> T.intercalate ", " (map toTextJSON v) <> "]"

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
  execute_ conn dictionarySchema
  execute_ conn "CREATE TABLE IF NOT EXISTS entry(id INTEGER PRIMARY KEY,glossary VARCHAR NOT NULL,definition_tags VARCHAR NOT NULL,term_tags VARCHAR NOT NULL,popularity INTEGER NOT NULL,rules VARCHAR NOT NULL,heading_id INTEGER NOT NULL REFERENCES heading ON DELETE RESTRICT ON UPDATE RESTRICT,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS tag(id INTEGER PRIMARY KEY,name VARCHAR NOT NULL,category VARCHAR NOT NULL,sorting_order INTEGER NOT NULL,notes VARCHAR NOT NULL,popularity INTEGER NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_tag_name UNIQUE (name,dictionary_id))"
  execute_ conn "CREATE TABLE IF NOT EXISTS kanji(id INTEGER PRIMARY KEY,kanji VARCHAR NOT NULL,onyomi VARCHAR NOT NULL,kunyomi VARCHAR NOT NULL,tags VARCHAR NOT NULL,meanings VARCHAR NOT NULL,stats BLOB NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_kanji UNIQUE (kanji,dictionary_id))"
  execute_ conn "CREATE TABLE IF NOT EXISTS kanji_frequency(id INTEGER PRIMARY KEY,kanji VARCHAR NOT NULL,data VARCHAR NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_kanji_frequency UNIQUE (kanji,data,dictionary_id))"
  execute_ conn "CREATE TABLE IF NOT EXISTS media(id INTEGER PRIMARY KEY,path VARCHAR NOT NULL,content BLOB NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_path UNIQUE (path,dictionary_id))"
  execute_ conn "CREATE TABLE IF NOT EXISTS term_meta(id INTEGER PRIMARY KEY,term VARCHAR NOT NULL,mode VARCHAR NOT NULL,data VARCHAR NOT NULL,dictionary_id INTEGER NOT NULL REFERENCES dictionary ON DELETE RESTRICT ON UPDATE RESTRICT,CONSTRAINT unique_term_meta UNIQUE (term,mode,data,dictionary_id))"
  execute_ conn "CREATE INDEX IF NOT EXISTS heading_reading_ix ON heading(reading)"
  execute_ conn "CREATE INDEX IF NOT EXISTS entry_heading_ix ON entry(heading_id)"
  where
    dictionarySchema =
      mconcat
        [ "CREATE TABLE IF NOT EXISTS dictionary(",
          "id INTEGER PRIMARY KEY,",
          "name VARCHAR NOT NULL,",
          "revision VARCHAR NOT NULL,",
          "sequenced BOOLEAN NOT NULL,",
          "import_date TIMESTAMP NOT NULL,",
          "terms_count INTEGER NOT NULL,",
          "term_meta_count INTEGER NOT NULL,",
          "kanji_count INTEGER NOT NULL,",
          "kanji_meta_count INTEGER NOT NULL,",
          "tag_meta_count INTEGER NOT NULL,",
          "media_count INTEGER NOT NULL,",
          "CONSTRAINT unique_dict_name UNIQUE (name))"
        ]

getDictionarySummaries :: Connection -> IO [Yomichan.Summary]
getDictionarySummaries conn = do
  rs <-
    query_ conn . mconcat $
      [ "SELECT name, terms_count, term_meta_count, tag_meta_count ",
        "FROM dictionary"
      ]
  pure . flip map rs $ \(dName, termsC, metaC, tagC) ->
    Yomichan.Summary
      dName
      ( Yomichan.Counts
          { terms = Yomichan.SummaryItemCount {total = termsC},
            termMeta = Yomichan.SummaryMetaCount {total = metaC},
            kanji = Yomichan.SummaryItemCount {total = 0},
            kanjiMeta = Yomichan.SummaryMetaCount {total = 0},
            tagMeta = Yomichan.SummaryItemCount {total = tagC},
            media = Yomichan.SummaryItemCount {total = 0}
          }
      )

insertIndex :: Connection -> Yomichan.Index -> IO Int64
insertIndex conn index = do
  now <- getCurrentTime
  let dictRow = (name, revision, sequenced, now)
  execute conn cmd dictRow
  lastInsertRowId conn
  where
    name = Yomichan.indexTitle index
    revision = Yomichan.indexRevision index
    sequenced = Yomichan.indexSequenced index
    cmd =
      mconcat
        [ "INSERT INTO dictionary (",
          "name, revision, sequenced, import_date, ",
          "terms_count, term_meta_count, kanji_count,",
          "kanji_meta_count, tag_meta_count, media_count",
          ") VALUES (?,?,?,?,0,0,0,0,0,0)"
        ]

updateDictionaryCounts :: Connection -> DictionaryId -> IO ()
updateDictionaryCounts conn dId = do
  termsC <- queryCount "entry"
  metaC <- queryCount "term_meta"
  tagC <- queryCount "tag"
  execute conn updateStmt (termsC, metaC, tagC, dId)
  where
    queryCount :: Text -> IO Int
    queryCount table = do
      let q =
            Util.sformat
              "SELECT COUNT(*) FROM {} r WHERE r.dictionary_id = {}"
              (table, dId)
      [Only c] <- query_ conn (Query q)
      pure c
    updateStmt =
      mconcat $
        [ "UPDATE dictionary ",
          "SET terms_count = ?, ",
          "term_meta_count = ?, ",
          "tag_meta_count = ? ",
          "WHERE id = ?"
        ]

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
    "INSERT INTO entry (glossary, definition_tags, term_tags, rules, popularity, rules, heading_id, dictionary_id) VALUES (?,?,?,?,?,?,?,?)"
    ( Yomichan.termDefinitions term,
      Yomichan.termDefinitionTags term,
      Yomichan.termTermTags term,
      Yomichan.termRuleIdentifiers term,
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
      Yomichan.termMetaData termMeta,
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
      forM_ (Yomichan.listTerms dict) (insertTerm conn dictId)
      forM_ (Yomichan.listTermMetas dict) (insertTermMeta conn dictId)
      runStream Yomichan.streamTags insertTag
      runStream Yomichan.streamKanji insertKanji
      runStream Yomichan.streamKanjiMetas insertKanjiMeta
      case Yomichan.getMedia dict of
        Left err -> Yomichan.throwImport err
        Right media ->
          forM_
            media
            ( \(fp, content) ->
                insertMedia conn dictId fp (LBS.toStrict content)
            )
      updateDictionaryCounts conn dictId
    Just _ -> Yomichan.throwImport "dictionary is already imported"

-- SEARCH ----------------------------------------------------------------------

data TermQuery = TermQuery
  { tmqTerms :: ![Text],
    tmqDictionaries :: ![Text],
    tmqMatchType :: !Text
  }

instance FromJSON TermQuery where
  parseJSON =
    A.withObject "" $ \o ->
      TermQuery
        <$> o .: "terms"
        <*> o .: "dictionaries"
        <*> o .: "matchType"

data TermResult = TermResult
  { termResultEntryId :: !Int64,
    termResultExpression :: !Text,
    termResultReading :: !(Maybe Text),
    termResultMatchSource :: !Text,
    termResultGlossary :: !Text,
    termResultDefinitionTags :: ![Text],
    termResultTermTags :: ![Text],
    termResultRules :: ![Text],
    termResultScore :: !Int,
    termResultDictionary :: !Text,
    termResultIndex :: !Int
  }
  deriving (Show, Eq)

-- TODO: use tmqMatchType
findTermsBulk :: Connection -> TermQuery -> IO [TermResult]
findTermsBulk _ (TermQuery {tmqDictionaries = []}) = pure []
findTermsBulk conn (TermQuery {..}) = do
  rows <- concat <$> Monad.forM (zip [0 ..] tmqTerms) (uncurry findTerm)
  pure $ map (\(i, text, row) -> mkResult i text row) rows
  where
    findTerm i text =
      let sqlQuery =
            mconcat
              [ "SELECT heading.term, heading.reading, entry.id,glossary,definition_tags,term_tags,rules,popularity,dictionary.name",
                " FROM heading INNER JOIN entry ON heading.id = entry.heading_id",
                " INNER JOIN dictionary ON dictionary.id = entry.dictionary_id",
                " WHERE ",
                "dictionary.name IN (",
                foldr1 (\x y -> x <> "," <> y) (fmap (\x -> "'" <> x <> "'") tmqDictionaries),
                ")",
                " AND (heading.term = ? OR heading.reading = ?)"
              ]
       in do
            rows <- query conn (Query sqlQuery) (text, text)
            pure (map (i,text,) rows)

    mkResult i text (term, reading, entryId, glossary, defTags, termTags, rules, score, dictionary) =
      let src = if term == text then "term" else "reading"
          defTagsList = filter (not . T.null) $ T.splitOn " " defTags
          termTagsList = filter (not . T.null) $ T.splitOn " " termTags
          rulesList = filter (not . T.null) $ T.splitOn " " rules
       in TermResult entryId term reading src glossary defTagsList termTagsList rulesList score dictionary i

instance ToTextJSON TermResult where
  toTextJSON (TermResult {..}) =
    mconcat
      [ "{",
        sformat ("\"id\": " % Formatting.int % ", ") termResultEntryId,
        sformat ("\"term\": \"" % Formatting.stext % "\", ") termResultExpression,
        sformat ("\"reading\": \"" % Formatting.stext % "\", ") reading,
        sformat ("\"matchSource\": \"" % Formatting.stext % "\", ") termResultMatchSource,
        "\"matchType\": \"exact\", ",
        sformat ("\"definitions\": " % Formatting.stext % ", ") termResultGlossary,
        sformat ("\"definitionTags\": " % Formatting.text % ", ") (A.encodeToLazyText termResultDefinitionTags),
        sformat ("\"termTags\": " % Formatting.text % ", ") (A.encodeToLazyText termResultTermTags),
        sformat ("\"rules\": " % Formatting.text % ", ") (A.encodeToLazyText termResultRules),
        sformat ("\"score\": " % Formatting.int % ", ") termResultScore,
        sformat ("\"dictionary\": \"" % Formatting.stext % "\", ") termResultDictionary,
        sformat ("\"index\": " % Formatting.int) termResultIndex,
        "}"
      ]
    where
      reading = Maybe.fromMaybe "" termResultReading

data TermMetaQuery = TermMetaQuery
  { tmmqTerms :: ![Text],
    tmmqDictionaries :: ![Text]
  }

instance FromJSON TermMetaQuery where
  parseJSON =
    A.withObject "" $ \o ->
      TermMetaQuery
        <$> o .: "terms"
        <*> o .: "dictionaries"

data TermMetaResult = TermMetaResult
  { termMetaResultIndex :: !Int,
    termMetaResultTerm :: !Text,
    termMetaResultMode :: !Text,
    termMetaResultData :: !Text,
    termMetaResultDictionary :: !Text
  }
  deriving (Show, Eq)

findTermMetaBulk :: Connection -> TermMetaQuery -> IO [TermMetaResult]
findTermMetaBulk _ (TermMetaQuery {tmmqDictionaries = []}) = pure []
findTermMetaBulk conn (TermMetaQuery {..}) = do
  rows <- concat <$> Monad.forM (zip [0 ..] tmmqTerms) (uncurry findTermMeta)
  pure $ map (uncurry mkResult) rows
  where
    findTermMeta i text =
      let sqlQuery =
            mconcat
              [ "SELECT term, mode, data, dictionary.name",
                " FROM term_meta INNER JOIN dictionary ON dictionary.id = term_meta.dictionary_id",
                " WHERE ",
                "dictionary.name IN (",
                foldr1 (\x y -> x <> "," <> y) (fmap (\x -> "'" <> x <> "'") tmmqDictionaries),
                ")",
                " AND (term_meta.term = ?)"
              ]
       in do
            rs <- query conn (Query sqlQuery) (Only text)
            pure $ map (i,) rs
    mkResult i (term, mode, data_, dictionary) = TermMetaResult i term mode data_ dictionary

instance ToTextJSON TermMetaResult where
  toTextJSON (TermMetaResult {..}) =
    mconcat
      [ "{",
        sformat ("\"index\": " % Formatting.int % ", ") termMetaResultIndex,
        sformat ("\"term\": \"" % Formatting.stext % "\", ") termMetaResultTerm,
        sformat ("\"mode\": \"" % Formatting.stext % "\", ") termMetaResultMode,
        sformat ("\"data\": " % Formatting.stext % ", ") termMetaResultData,
        sformat ("\"dictionary\": \"" % Formatting.stext % "\"") termMetaResultDictionary,
        "}"
      ]

data DictionaryAndQueryRequest = DictionaryAndQueryRequest
  { dqrQuery :: Text,
    dqrDictionary :: Text
  }
  deriving (Show)

instance FromJSON DictionaryAndQueryRequest where
  parseJSON =
    A.withObject
      ""
      (\o -> DictionaryAndQueryRequest <$> o .: "query" <*> o .: "dictionary")

data Tag = Tag
  { tagName :: !Text,
    tagCategory :: !Text,
    tagOrder :: !Int,
    tagNotes :: !Text,
    tagScore :: !Int,
    tagDictionary :: !Text
  }
  deriving (Show)
  deriving (ToTextJSON) via AesonValue Tag

instance ToJSON Tag where
  toJSON Tag {..} =
    A.object
      [ "name" A..= tagName,
        "category" A..= tagCategory,
        "order" A..= tagOrder,
        "notes" A..= tagNotes,
        "score" A..= tagScore,
        "dictionary" A..= tagDictionary
      ]

findTagMetaBulk :: Connection -> [DictionaryAndQueryRequest] -> IO [Tag]
findTagMetaBulk conn = fmap Maybe.catMaybes . mapM findTagMeta
  where
    findTagMeta :: DictionaryAndQueryRequest -> IO (Maybe Tag)
    findTagMeta (DictionaryAndQueryRequest {..}) =
      let sql =
            mconcat
              [ "SELECT tag.name, category, sorting_order, notes, popularity ",
                "FROM tag INNER JOIN dictionary ON dictionary.id = tag.dictionary_id ",
                "WHERE tag.name = ? AND dictionary.name = ? ",
                "LIMIT 1"
              ]
       in query conn (Query sql) (dqrQuery, dqrDictionary) <&> \case
            [(name, category, order, notes, score)] ->
              Just (Tag name category order notes score dqrDictionary)
            _ -> Nothing

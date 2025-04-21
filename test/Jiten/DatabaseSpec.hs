module Jiten.DatabaseSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Jiten.Database as Db
import qualified Jiten.TestUtil as TestUtil
import qualified Jiten.Util as Util
import qualified Jiten.Yomichan.Dictionary as Yomi
import Test.Hspec (Spec, around, describe, it, shouldBe)

spec :: Spec
spec =
  around withValidDictionary1 $ do
    describe "on test dictionary 'valid-dictionary1.zip'" $ do
      describe "getDictionaries" $ do
        it "returns the correct list of dictionaries" $ \conn -> do
          dicts <- Db.getDictionaries conn
          dicts `shouldBe` [(1, "Test Dictionary")]
      describe "getDictionarySummaries" $ do
        it "returns the correct dictionary summaries" $ \conn -> do
          summaries <- Db.getDictionarySummaries conn
          let pp = unlines . map (TestUtil.pprintJson . Util.encodeStrict)
          pure (TestUtil.mkGolden pp "getDictionarySummaries" summaries)
      describe "findTermsBulk" $ do
        it "returns an empty list when disabling the dictionary" $ \conn -> do
          results <- Db.findTermsBulk conn (Db.TermQuery ["打つ"] [] "exact")
          results `shouldBe` []
        describe "and toTextJSON" $ do
          it "render JSON results correctly" $ \conn -> do
            let qs = ["打", "打ち込む", "お手前", "土木工事", "好き"]
            results <- Db.findTermsBulk conn (Db.TermQuery qs [dict] "exact")
            let pp = unlines . map (TestUtil.pprintJson . Db.toTextJSON)
            pure (TestUtil.mkGolden pp "findTermsBulk_json" results)
      describe "findTermMetaBulk" $ do
        describe "and toTextJSON" $ do
          it "render JSON results correctly" $ \conn -> do
            let qs = ["打", "打ち込む", "お手前", "土木工事", "好き"]
            results <- Db.findTermMetaBulk conn (Db.TermMetaQuery qs [dict])
            let pp = unlines . map (TestUtil.pprintJson . Db.toTextJSON)
            pure (TestUtil.mkGolden pp "findTermMetaBulk_json" results)
      describe "findTagMetaBulk" $ do
        describe "and toTextJSON" $ do
          it "render JSON results correctly" $ \conn -> do
            let qs = map (flip Db.DictionaryAndQueryRequest "Test Dictionary") tags
            results <- Db.findTagMetaBulk conn qs
            let pp = unlines . map (TestUtil.pprintJson . Db.toTextJSON)
            pure (TestUtil.mkGolden pp "findTagMetaBulk" results)
  where
    dict = "Test Dictionary"
    dictPath = "./test/valid-dictionary1.zip" :: FilePath

    withValidDictionary1 f =
      Sql.withConnection ":memory:" $ \conn -> do
        Db.initDatabase conn
        Yomi.openArchiveFile dictPath >>= Db.insertDictionary conn
        f conn

    tags =
      [ "E1",
        "E2",
        "P",
        "n",
        "vt",
        "abbr",
        "K1",
        "K2",
        "kstat1",
        "kstat2",
        "kstat3",
        "kstat4",
        "kstat5",
        "P1",
        "P2"
      ]

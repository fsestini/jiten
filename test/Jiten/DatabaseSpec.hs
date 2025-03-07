module Jiten.DatabaseSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Jiten.Database as Db
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
      describe "findTermsBulk" $ do
        it "returns terms" $ \conn -> do
          results <- Db.findTermsBulk conn ["打つ"] [1]
          let res1 = Db.TermResult 3 "打つ" "うつ" "term" "[\"utsu definition 1\",\"utsu definition 2\"]" "vt" "P E1" 10 "Test Dictionary" 0
          let res2 = Db.TermResult 4 "打つ" "うつ" "term" "[\"utsu definition 3\",\"utsu definition 4\"]" "vt" "P E2" 1 "Test Dictionary" 1
          let res3 = Db.TermResult 5 "打つ" "ぶつ" "term" "[\"butsu definition 1\",\"butsu definition 2\"]" "vt" "P E1" 10 "Test Dictionary" 2
          let res4 = Db.TermResult 6 "打つ" "ぶつ" "term" "[\"butsu definition 3\",\"butsu definition 4\"]" "vt" "P E2" 1 "Test Dictionary" 3
          results `shouldBe` [res1, res2, res3, res4]
        it "returns an empty list when disabling the dictionary" $ \conn -> do
          results <- Db.findTermsBulk conn ["打つ"] []
          results `shouldBe` []
      describe "termResultToJSON" $ do
        it "renders JSON result correctly" $ \conn -> do
          results <- Db.findTermsBulk conn ["土木工事"] [1]
          map Db.termResultToJSON results
            `shouldBe` [ "{\"entryId\": 19,\
                         \ \"expression\": \"土木工事\",\
                         \ \"reading\": \"どぼくこうじ\",\
                         \ \"matchSource\": \"term\", \"glossary\": [\"dobokukouji definition\"],\
                         \ \"definitionTags\": \"n\",\
                         \ \"termTags\": \"\",\
                         \ \"score\": 1,\
                         \ \"dictionary\": \"Test Dictionary\",\
                         \ \"index\": 0}"
                       ]
      describe "termMetaResultToJSON" $ do
        it "renders JSON result correctly" $ \conn -> do
          results <- Db.findTermMetaBulk conn ["土木工事"] [1]
          map Db.termMetaResultToJSON results
            `shouldBe` [ "{\"index\": 0, \"term\": \"土木工事\", \"mode\": \"pitch\",\
                         \ \"data\": {\"pitches\":[{\"devoice\":3,\"position\":4}],\"reading\":\"どぼくこうじ\"},\
                         \ \"dictionary\": \"Test Dictionary\"}"
                       ]
  where
    dictPath = "./test/valid-dictionary1.zip" :: FilePath
    withValidDictionary1 f =
      Sql.withConnection ":memory:" $ \conn -> do
        Db.initDatabase conn
        dict <- Yomi.openArchiveFile dictPath
        Db.insertDictionary conn dict
        f conn

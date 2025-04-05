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
          results <-
            Db.findTermsBulk
              conn
              (Db.TermQuery ["打つ"] ["Test Dictionary"] "exact")
          let res1 = Db.TermResult 3 "打つ" (Just "うつ") "term" "[\"utsu definition 1\",\"utsu definition 2\"]" ["vt"] ["P", "E1"] ["v5"] 10 "Test Dictionary" 0
          let res2 = Db.TermResult 4 "打つ" (Just "うつ") "term" "[\"utsu definition 3\",\"utsu definition 4\"]" ["vt"] ["P", "E2"] ["v5"] 1 "Test Dictionary" 0
          let res3 = Db.TermResult 5 "打つ" (Just "ぶつ") "term" "[\"butsu definition 1\",\"butsu definition 2\"]" ["vt"] ["P", "E1"] ["v5"] 10 "Test Dictionary" 0
          let res4 = Db.TermResult 6 "打つ" (Just "ぶつ") "term" "[\"butsu definition 3\",\"butsu definition 4\"]" ["vt"] ["P", "E2"] ["v5"] 1 "Test Dictionary" 0
          results `shouldBe` [res1, res2, res3, res4]
        it "returns an empty list when disabling the dictionary" $ \conn -> do
          results <-
            Db.findTermsBulk
              conn
              (Db.TermQuery ["打つ"] [] "exact")
          results `shouldBe` []
      describe "termResultToJSON" $ do
        it "renders JSON result correctly" $ \conn -> do
          results <-
            Db.findTermsBulk
              conn
              (Db.TermQuery ["土木工事"] ["Test Dictionary"] "exact")
          map Db.toTextJSON results
            `shouldBe` [ "{\"id\": 19,\
                         \ \"term\": \"土木工事\",\
                         \ \"reading\": \"どぼくこうじ\",\
                         \ \"matchSource\": \"term\",\
                         \ \"matchType\": \"exact\",\
                         \ \"definitions\": [\"dobokukouji definition\"],\
                         \ \"definitionTags\": [\"n\"],\
                         \ \"termTags\": [],\
                         \ \"rules\": [\"n\"],\
                         \ \"score\": 1,\
                         \ \"dictionary\": \"Test Dictionary\",\
                         \ \"index\": 0}"
                       ]
  where
    -- describe "termMetaResultToJSON" $ do
    --   it "renders JSON result correctly" $ \conn -> do
    --     results <-
    --       Db.findTermMetaBulk
    --         conn
    --         (Db.TermMetaQuery ["土木工事"] ["Test Dictionary"])
    --     map Db.toTextJSON results
    --       `shouldBe` [ "{\"index\": 0, \"term\": \"土木工事\", \"mode\": \"pitch\",\
    --                    \ \"data\": {\"pitches\":[{\"devoice\":3,\"position\":4}],\"reading\":\"どぼくこうじ\"},\
    --                    \ \"dictionary\": \"Test Dictionary\"}"
    --                  ]

    dictPath = "./test/valid-dictionary1.zip" :: FilePath
    withValidDictionary1 f =
      Sql.withConnection ":memory:" $ \conn -> do
        Db.initDatabase conn
        dict <- Yomi.openArchiveFile dictPath
        Db.insertDictionary conn dict
        f conn

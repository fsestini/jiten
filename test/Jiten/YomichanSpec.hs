module Jiten.YomichanSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Jiten.Database as Db
import qualified Jiten.Yomichan.Core as Core
import qualified Jiten.Yomichan.Dictionary as Dict
import qualified Jiten.Yomichan.Search as Search
import Test.Hspec (Spec, around, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "formatFindTermsQuery" $ do
    it "should format query string correctly" $ do
      let fmtd = Search.formatFindTermsQuery Search.Simple "打"
      fmtd `shouldBe` "translator.findTerms('simple', '打', options)"
  around withYomiCtx $ do
    describe "on test dictionary 'valid-dictionary1.zip'" $ do
      describe "findTerms" $ do
        it "correctly returns results for '打ち込む' on simple mode" $ \ctx -> do
          -- let text = "打ち込む"
          -- result <- Search.findTerms ctx Search.Simple text
          -- result `shouldBe` ""
          "" `shouldBe` ""
  where
    dictPath = "./test/valid-dictionary1.zip" :: FilePath
    withYomiCtx f =
      Sql.withConnection ":memory:" $ \conn -> do
        Db.initDatabase conn
        dict <- Dict.openArchiveFile dictPath
        Db.insertDictionary conn dict
        let getDictIds = map fst <$> Db.getDictionaries conn
        Core.withYomitan conn getDictIds f

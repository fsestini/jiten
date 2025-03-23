module Jiten.YomichanSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Jiten.Database as Db
import qualified Jiten.TestUtil as TestUtil
import qualified Jiten.Yomichan.Core as Core
import qualified Jiten.Yomichan.Dictionary as Dict
import qualified Jiten.Yomichan.Search as Search
import Test.Hspec (Spec, around, describe, it, shouldBe)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Pretty as Html.Pretty

pprintHTML :: [Html] -> String
pprintHTML = unlines . map Html.Pretty.renderHtml

spec :: Spec
spec = do
  describe "formatFindTermsQuery" $ do
    it "should format query string correctly" $ do
      let fmtd = Search.formatFindTermsQuery Search.Simple "打"
      fmtd `shouldBe` "JSON.stringify(translator.findTerms('simple', '打', options))"
  around withYomiCtx $ do
    describe "on test dictionary 'valid-dictionary1.zip'" $ do
      describe "findTerms" $ do
        it "correctly returns results for '打ち込む' on simple mode" $ \ctx -> do
          let text = "打ち込む"
          result <- Search.findTerms ctx Search.Simple text
          pure (TestUtil.mkGolden TestUtil.pprintJson "findTerms_simple_1" result)
      describe "findTermsHTML" $ do
        it "打ち込む - simple" $ \ctx -> do
          let text = "打ち込む"
          result <- Search.findTermsHTML ctx Search.Simple text
          pure (TestUtil.mkGolden pprintHTML "findTermsHTML_simple_1" result)
  where
    dictPath = "./test/valid-dictionary1.zip" :: FilePath
    withYomiCtx f =
      Sql.withConnection ":memory:" $ \conn -> do
        Db.initDatabase conn
        dict <- Dict.openArchiveFile dictPath
        Db.insertDictionary conn dict
        Core.withYomitan conn $ \ctx -> do
          dicts <- map snd <$> Db.getDictionaries conn
          Search.setOptions ctx dicts (Just "Test Dictionary") >> f ctx

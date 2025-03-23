module Jiten.YomichanSpec where

import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A.Pretty
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Database.SQLite.Simple as Sql
import qualified Jiten.Database as Db
import qualified Jiten.Yomichan.Core as Core
import qualified Jiten.Yomichan.Dictionary as Dict
import qualified Jiten.Yomichan.Search as Search
import Test.Hspec (Spec, around, describe, it, shouldBe)
import Test.Hspec.Golden

pprintJson :: Text -> String
pprintJson json =
  case A.decodeStrictText json :: Maybe Value of
    Nothing -> T.unpack json
    Just decoded -> LT.unpack . LTE.decodeUtf8 . A.Pretty.encodePretty $ decoded

mkGolden :: (a -> String) -> String -> a -> Golden String
mkGolden pp name actualOutput =
  ( defaultGolden
      name
      (pp actualOutput)
  )
    { goldenFile = ".golden/" <> name <> ".expected",
      actualFile = Just (".golden/" <> name <> ".actual"),
      failFirstTime = True
    }

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
          pure (mkGolden pprintJson "findTerms_simple_1" result)
  where
    dictPath = "./test/valid-dictionary1.zip" :: FilePath
    withYomiCtx f =
      Sql.withConnection ":memory:" $ \conn -> do
        Db.initDatabase conn
        dict <- Dict.openArchiveFile dictPath
        Db.insertDictionary conn dict
        let getDictIds = map fst <$> Db.getDictionaries conn
        Core.withYomitan conn getDictIds $ \ctx -> do
          dicts <- map snd <$> Db.getDictionaries conn
          Search.setOptions ctx dicts >> f ctx

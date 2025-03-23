module Jiten.Server where

import Data.List (isPrefixOf)
import qualified Database.SQLite.Simple as Sql
import qualified Jiten.Database as Db
import qualified Jiten.Yomichan.Core as Core
import qualified Jiten.Yomichan.Search as Search
import qualified Jiten.Yomichan.SearchPageTemplate as SearchPageTemplate
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Network.Wai.Middleware.Static as Static
import qualified Paths_jiten
import Text.Blaze ((!))
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (scotty)
import qualified Web.Scotty as Scotty

data ServerConfig = ServerConfig
  { cfgPort :: !Int,
    cfgYomiCtx :: !Core.YomiContext,
    cfgDataDir :: !FilePath
  }

serve :: ServerConfig -> IO ()
serve cfg = scotty (cfgPort cfg) $ do
  Scotty.middleware (Static.staticPolicy policy . simpleCors)
  Scotty.get "/" $ do
    Scotty.html . Blaze.renderHtml $ do
      H.h1 "Jiten"
      H.a ! A.href "/search" $ "Search"
  Scotty.get "/search" $ do
    qMay <- Scotty.queryParamMaybe "query"
    contents <-
      case qMay of
        Just q ->
          Scotty.liftIO (Search.findTermsHTML (cfgYomiCtx cfg) Search.Simple q)
        Nothing -> pure [H.text "No results"]
    Scotty.html . Blaze.renderHtml . SearchPageTemplate.instantiate $
      mconcat contents
  Scotty.get "/api/status" $ do
    Scotty.text "Ok."
  where
    policy = Static.policy $ \s ->
      if ("css" `isPrefixOf` s) || ("images" `isPrefixOf` s)
        then Just (cfgDataDir cfg <> "/" <> "vendor/yomitan/ext/" <> s)
        else Nothing

runServer :: IO ()
runServer = do
  dataDir <- Paths_jiten.getDataDir
  Sql.withConnection "jiten.db" $ \conn -> do
    Db.initDatabase conn
    dicts <- Db.getDictionaries conn
    Core.withYomitan conn (pure (map fst dicts)) $ \ctx -> do
      Search.setOptions ctx (map snd dicts)
      serve
        ( ServerConfig
            { cfgPort = 3000,
              cfgYomiCtx = ctx,
              cfgDataDir = dataDir
            }
        )

module Jiten.Server where

import Control.Concurrent (forkIO, forkOS, rtsSupportsBoundThreads)
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (forever, void)
import Data.Function ((&))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sql
import qualified Jiten.Database as Db
import qualified Jiten.Server.SearchPage as SearchPage
import qualified Jiten.Yomichan.Core as Core
import qualified Jiten.Yomichan.Search as Search
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Network.Wai.Middleware.Static as Static
import qualified Paths_jiten
import Text.Blaze (Markup, (!))
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (scotty)
import qualified Web.Scotty as Scotty

data ServerConfig = ServerConfig
  { cfgPort :: !Int,
    cfgYomiVar :: MVar (Text, MVar [Markup]),
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
    queryParamMay <- Scotty.queryParamMaybe "query"
    offset <- fromMaybe (0 :: Int) <$> Scotty.queryParamMaybe "offset"
    (queryParam, contents) <-
      case queryParamMay of
        Just queryParam ->
          let query = T.drop offset queryParam
           in (queryParam,) <$> findTermsHTML query
        Nothing -> pure (mempty, [])
    Scotty.html . Blaze.renderHtml $
      SearchPage.instantiate contents queryParam offset
  Scotty.get "/api/status" $ do
    Scotty.text "Ok."
  where
    findTermsHTML :: Text -> Scotty.ActionM [Markup]
    findTermsHTML q = Scotty.liftIO $ do
      v <- MVar.newEmptyMVar
      MVar.putMVar (cfgYomiVar cfg) (q, v)
      MVar.readMVar v
    policy = Static.policy $ \s ->
      if ("css" `isPrefixOf` s) || ("images" `isPrefixOf` s)
        then Just (cfgDataDir cfg <> "/" <> "vendor/yomitan/ext/" <> s)
        else Nothing

runYomi :: MVar (Text, MVar [Markup]) -> IO ()
runYomi v =
  Sql.withConnection "jiten.db" $ \conn -> do
    Db.initDatabase conn
    dicts <- Db.getDictionaries conn
    Core.withYomitan conn $ \ctx -> do
      -- TODO: set frequency dictionary
      Search.setOptions ctx (map snd dicts) Nothing
      forever $ do
        (query, returnVar) <- MVar.takeMVar v
        result <- Search.findTermsHTML ctx Search.Split query
        MVar.putMVar returnVar result

{-
The quickjs runtime does not like to be called from an OS thread other than the
one it was started in. To avoid crashing it, and still be able to call js
functions within a multithreaded server, we spawn an instance of Yomitan in
its own dedicated bound thread, and interact with it from the server's request
handlers via MVars.

See https://github.com/haskell-github-trust/quickjs-hs/blob/aa516edfe67611d1e5fb469cb77f102030d57351/src/Quickjs.hs#L589
-}
runServer :: IO ()
runServer = do
  dataDir <- Paths_jiten.getDataDir
  yomiVar <- MVar.newEmptyMVar
  if rtsSupportsBoundThreads
    then void (forkOS (runYomi yomiVar))
    else void (forkIO (runYomi yomiVar))
  serve
    ( ServerConfig
        { cfgPort = 3000,
          cfgYomiVar = yomiVar,
          cfgDataDir = dataDir
        }
    )

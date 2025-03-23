module Jiten.CLI where

import Control.Monad (forM_, join)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import qualified Database.SQLite.Simple as Sql
import qualified Jiten.Database as Db
import qualified Jiten.Server as Server
import qualified Jiten.Util as Util
import qualified Jiten.Yomichan.Core as Core
import qualified Jiten.Yomichan.Dictionary as Yomi
import qualified Jiten.Yomichan.Search as Search
import Options.Applicative (Parser, (<**>))
import qualified Options.Applicative as Cmd
import qualified Text.Blaze.Renderer.Text as Blaze

data QueryFormat
  = FormatJSON
  | FormatHTML
  | FormatDeinflections
  deriving (Show)

main :: IO ()
main = join $ Cmd.execParser (Cmd.info (opts <**> Cmd.helper) programInfo)
  where
    programInfo =
      Cmd.fullDesc
        <> Cmd.progDesc "Jiten"
        <> Cmd.header "Jiten - query Japanese dictionaries in Yomichan format."

    opts :: Parser (IO ())
    opts =
      Cmd.subparser
        ( Cmd.command
            "import"
            ( Cmd.info
                (importDict <$> Cmd.argument Cmd.str (Cmd.metavar "PATH") <**> Cmd.helper)
                (Cmd.progDesc "Import a Yomichan dictionary")
            )
            <> Cmd.command
              "list"
              ( Cmd.info
                  (pure listDicts <**> Cmd.helper)
                  (Cmd.progDesc "List all imported dictionaries")
              )
            <> Cmd.command
              "drop"
              ( Cmd.info
                  (dropDict <$> Cmd.argument Cmd.str (Cmd.metavar "NAME") <**> Cmd.helper)
                  (Cmd.progDesc "Remove a dictionary by name")
              )
            <> Cmd.command
              "serve"
              ( Cmd.info
                  (pure serve <**> Cmd.helper)
                  (Cmd.progDesc "Start the dictionary server")
              )
            <> Cmd.command
              "query"
              ( Cmd.info
                  (query <$> queryFormatOption <*> Cmd.argument Cmd.str (Cmd.metavar "QUERY") <**> Cmd.helper)
                  (Cmd.progDesc "Search for terms")
              )
        )

queryFormatOption :: Parser QueryFormat
queryFormatOption =
  Cmd.option
    readFormat
    ( Cmd.long "format"
        <> Cmd.short 'f'
        <> Cmd.help "Output format: json, html, or deinflections"
        <> Cmd.metavar "FORMAT"
        <> Cmd.value FormatJSON
        <> Cmd.showDefaultWith (const "json")
    )
  where
    readFormat = Cmd.eitherReader $ \s -> case s of
      "json" -> Right FormatJSON
      "html" -> Right FormatHTML
      "deinflections" -> Right FormatDeinflections
      _ -> Left $ "Unknown format: " ++ s ++ ". Use `json`, `html`, or `deinflections`."

importDict :: FilePath -> IO ()
importDict fp = Sql.withConnection "jiten.db" $ \conn -> do
  Db.initDatabase conn
  dict <- Yomi.openArchiveFile fp
  Db.insertDictionary conn dict

listDicts :: IO ()
listDicts = do
  Sql.withConnection "jiten.db" $ \conn -> do
    Db.initDatabase conn
    dicts <- Db.getDictionaries conn
    if null dicts
      then putStrLn "No dictionaries imported."
      else forM_ dicts $ TIO.putStrLn . Util.sformat "{}: {}"

dropDict :: Text -> IO ()
dropDict _ = putStrLn "NIY"

serve :: IO ()
serve = Server.runServer

query :: QueryFormat -> Text -> IO ()
query FormatJSON txt = queryResults txt
query FormatDeinflections txt = queryDeinflections txt
query FormatHTML txt = queryHTML txt

queryHTML :: Text -> IO ()
queryHTML q =
  Sql.withConnection "jiten.db" $ \conn -> do
    Db.initDatabase conn
    dicts <- Db.getDictionaries conn
    Core.withYomitan conn (pure (map fst dicts)) $ \ctx -> do
      Search.setOptions ctx (map snd dicts)
      result <- Search.findTermsHTML ctx Search.Simple q
      let rendered = map Blaze.renderMarkup result
      forM_ rendered LTIO.putStrLn

queryDeinflections :: Text -> IO ()
queryDeinflections q =
  Sql.withConnection "jiten.db" $ \conn -> do
    Db.initDatabase conn
    dicts <- Db.getDictionaries conn
    Core.withYomitan conn (pure (map fst dicts)) $ \ctx -> do
      Search.setOptions ctx (map snd dicts)
      result <- Search.getAlgorithmDeinflections ctx q
      TIO.putStrLn result

queryResults :: Text -> IO ()
queryResults q =
  Sql.withConnection "jiten.db" $ \conn -> do
    Db.initDatabase conn
    dicts <- Db.getDictionaries conn
    Core.withYomitan conn (pure (map fst dicts)) $ \ctx -> do
      Search.setOptions ctx (map snd dicts)
      result <- Search.findTerms ctx Search.Simple q
      TIO.putStrLn result

module Jiten.CLI where

import Control.Monad (join)
import Data.Text (Text)
import qualified Database.SQLite.Simple as Sql
import qualified Jiten.Database as Db
import qualified Jiten.Yomichan.Dictionary as Yomi
import Options.Applicative (Parser)
import qualified Options.Applicative as Cmd

main :: IO ()
main = join $ Cmd.execParser (Cmd.info opts Cmd.idm)
  where
    opts :: Parser (IO ())
    opts =
      Cmd.subparser
        ( Cmd.command "import" (Cmd.info (importDict <$> Cmd.argument Cmd.str (Cmd.metavar "PATH")) Cmd.idm)
            <> Cmd.command "list" (Cmd.info (pure listDicts) Cmd.idm)
            <> Cmd.command "drop" (Cmd.info (dropDict <$> Cmd.argument Cmd.str (Cmd.metavar "NAME")) Cmd.idm)
            <> Cmd.command "serve" (Cmd.info (pure serve) Cmd.idm)
        )

importDict :: FilePath -> IO ()
importDict fp = Sql.withConnection "jiten.db" $ \conn -> do
  Db.initDatabase conn
  dict <- Yomi.openArchiveFile fp
  Db.insertDictionary conn dict

listDicts :: IO ()
listDicts = putStrLn "NIY"

dropDict :: Text -> IO ()
dropDict _ = putStrLn "NIY"

serve :: IO ()
serve = putStrLn "NIY"

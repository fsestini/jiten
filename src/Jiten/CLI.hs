module Jiten.CLI where

import Control.Monad (join)
import Data.Text (Text)
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
importDict _ = putStrLn "NIY"

listDicts :: IO ()
listDicts = putStrLn "NIY"

dropDict :: Text -> IO ()
dropDict _ = putStrLn "NIY"

serve :: IO ()
serve = putStrLn "NIY"

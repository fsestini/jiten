module Jiten.TestUtil where

import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A.Pretty
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
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

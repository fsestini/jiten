{-# LANGUAGE TypeApplications #-}

module Jiten.TestUtil where

import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A.Pretty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Test.Hspec.Golden

pprintJson :: Text -> String
pprintJson =
  LT.unpack
    . LTE.decodeUtf8
    . A.Pretty.encodePretty
    . fromMaybe (error "pprintJson: failed to parse JSON")
    . A.decodeStrictText @Value

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

module Jiten.Util (sformat, strFormat, findJust) where

import Data.Foldable (asum)
import Data.Text (Text)
import qualified Data.Text.Format as Format
import qualified Data.Text.Format.Params as Format
import qualified Data.Text.Lazy as LT

sformat :: (Format.Params ps) => Format.Format -> ps -> Text
sformat fmt = LT.toStrict . Format.format fmt

strFormat :: (Format.Params ps) => Format.Format -> ps -> String
strFormat fmt = LT.unpack . Format.format fmt

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f xs = asum (map f xs)

module Jiten.Util (sformat, strFormat, findJust, decodeJSON) where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Foldable (asum)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text.Format as Format
import qualified Data.Text.Format.Params as Format
import qualified Data.Text.Lazy as LT

sformat :: (Format.Params ps) => Format.Format -> ps -> Text
sformat fmt = LT.toStrict . Format.format fmt

strFormat :: (Format.Params ps) => Format.Format -> ps -> String
strFormat fmt = LT.unpack . Format.format fmt

-- | Return the first 'Just' value obtained by applying the function @f@
-- to elements of the list.
-- If no element produces a 'Just' value, returns 'Nothing'.
findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f xs = asum (map f xs)

-- | Decode JSON text into a value of type @a@.
-- Throws an error if decoding fails.
-- Uses 'Data.Aeson.decodeStrictText' internally.
decodeJSON :: (FromJSON a) => Text -> a
decodeJSON =
  Maybe.fromMaybe (error "decodeJSON: failed to decode JSON")
    . A.decodeStrictText

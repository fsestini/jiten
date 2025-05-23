module Jiten.Util
  ( sformat,
    strFormat,
    findJust,
    decodeJSON,
    postfixes,
    encodeStrict,
    encodeStrictText,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (asum)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
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

-- | Return all the postfixes of a given string.
--
-- Example:
-- @
-- postfixes "abc" == ["abc", "bc", "c", ""]
-- @
postfixes :: Text -> [Text]
postfixes t = [T.drop n t | n <- [0 .. T.length t]]

-- | Encode to strict 'Text'.
encodeStrictText :: (ToJSON a) => a -> Text
encodeStrictText = LT.toStrict . A.encodeToLazyText

-- | Encode to strict 'ByteString'.
encodeStrict :: (ToJSON a) => a -> ByteString
encodeStrict = LBS.toStrict . A.encode

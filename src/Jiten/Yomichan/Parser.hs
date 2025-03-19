{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Jiten.Yomichan.Parser
  ( JsonValue (..),
    unfoldRow,
    unfoldBank,
    encode,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as AP
import qualified Data.Attoparsec.ByteString as Atto
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import Data.Word (Word8)
import Jiten.Util (unfold)

-- | JSON values.
--
-- Only strings, numbers, and null values are fully structurally represented,
-- while objects and arrays are represented as unparsed 'ByteString's.
-- 'JsonValue' terms can thus represent the result of partially parsing large
-- JSON structures up to the desired level of nesting, in situations where
-- parsing into a full-fledged JSON type is not needed and/or inefficient.
data JsonValue = Null | String !Text | Number !Scientific | Raw !ByteString
  deriving (Show)

data Scan = Scan {totalScanned :: !Int, currentlyOpen :: !Int}

consumeBySep :: Word8 -> Word8 -> ByteString -> Maybe (ByteString, ByteString)
consumeBySep open close input =
  case consumeChunks (Scan 0 0) input of
    Scan {totalScanned = 0, currentlyOpen = _} -> Nothing
    Scan {totalScanned = tot, currentlyOpen = co} ->
      if co == 0
        then Just (BS.splitAt tot input)
        else Nothing
  where
    consumeChunks sc bs =
      case BS.findIndex (== close) bs of
        Nothing -> sc
        Just ix ->
          let (currentChunk, nextChunk) = BS.splitAt (ix + 1) bs
              !newSc = consumeChunk sc currentChunk
           in if currentlyOpen newSc > 0
                then consumeChunks newSc nextChunk
                else newSc
    consumeChunk =
      BS.foldl'
        ( \(Scan {totalScanned = tot, currentlyOpen = co}) c ->
            let newTot = tot + 1
                newCo
                  | c == open = co + 1
                  | c == close = co - 1
                  | otherwise = co
             in Scan newTot newCo
        )

consumeArray :: ByteString -> Maybe (ByteString, ByteString)
consumeArray = consumeBySep 0x5B 0x5D

consumeObject :: ByteString -> Maybe (ByteString, ByteString)
consumeObject = consumeBySep 0x7B 0x7D

unfoldAll :: (ByteString -> Maybe (a, ByteString)) -> (ByteString, Bool) -> [a]
unfoldAll f = unfold $ \(bs, inMiddle) ->
  let sepP = if inMiddle then skipComma else skipOpenBracket
   in case Atto.parse (skipSpace >> sepP >> skipSpace) bs of
        Atto.Done rest () -> fmap (second (,True)) (f rest)
        _ -> Nothing
  where
    skipWord8 w = Atto.skip (== w) >> skipSpace
    skipOpenBracket = skipWord8 0x5B
    skipComma = skipWord8 0x2C
    skipSpace = Atto.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

consumeValue :: ByteString -> Maybe (JsonValue, ByteString)
consumeValue bs =
  extractResult (Atto.parse simpleValue bs)
    <|> fmap (first Raw) (consumeArray bs)
    <|> fmap (first Raw) (consumeObject bs)
  where
    extractResult (Atto.Done rest x) = Just (x, rest)
    extractResult _ = Nothing
    simpleValue =
      (Atto.string "null" >> pure Null)
        <|> fmap String AP.jstring
        <|> fmap Number AP.scientific

-- | Given a raw string that is known to (1) be a JSON array, and
-- (2) only contain arrays as its elements, return a lazily-constructed list
-- of the unparsed array elements of the outer array.
unfoldBank :: ByteString -> [ByteString]
unfoldBank bs = unfoldAll consumeArray (bs, False)

-- | Given a raw string that is known to be a JSON array,
-- return a lazily-constructed list of all its elements.
unfoldRow :: ByteString -> [JsonValue]
unfoldRow bs = unfoldAll consumeValue (bs, False)

-- | Encode a 'JsonValue' to strict 'ByteString'.
encode :: JsonValue -> ByteString
encode Null = "null"
encode (String str) = LBS.toStrict (A.encode str)
encode (Raw raw) = raw
encode (Number sc) =
  either
    (BS8.pack . show @Double)
    (BS8.pack . show @Int)
    (Scientific.floatingOrInteger sc)

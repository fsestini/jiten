{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Jiten.Yomichan.Summary where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype SummaryItemCount = SummaryItemCount
  { total :: Int
  }
  deriving (Generic)

-- TODO: [key: string]:: number
newtype SummaryMetaCount = SummaryMetaCount
  { total :: Int
  }
  deriving (Generic)

data Counts = Counts
  { terms :: SummaryItemCount,
    termMeta :: SummaryMetaCount,
    kanji :: SummaryItemCount,
    kanjiMeta :: SummaryMetaCount,
    tagMeta :: SummaryItemCount,
    media :: SummaryItemCount
  }
  deriving (Generic)

data Summary = Summary
  { title :: Text,
    counts :: Counts
  }
  deriving (Generic)

instance ToJSON SummaryItemCount

instance ToJSON SummaryMetaCount

instance ToJSON Counts

instance ToJSON Summary

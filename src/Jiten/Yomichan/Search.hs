module Jiten.Yomichan.Search where

import Control.Monad (void)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import qualified Data.Text.Format as Format
import qualified Data.Text.Lazy as LT
import Jiten.Yomichan.Core (YomiContext)
import qualified Jiten.Yomichan.Core as Core

data FindTermsMode = Group | Merge | Split | Simple

textMode :: FindTermsMode -> Text
textMode Group = "group"
textMode Merge = "merge"
textMode Split = "split"
textMode Simple = "simple"

setOptions :: YomiContext -> [Text] -> IO ()
setOptions ctx dictionaries = do
  let dictsList = encodeToLazyText dictionaries
      stmtText =
        Format.format
          "var options = mkOptions({})"
          (Format.Only dictsList)
      stmt = LT.unpack stmtText
  void (Core.jsEvalStr ctx stmt)

formatFindTermsQuery :: FindTermsMode -> Text -> LT.Text
formatFindTermsQuery mode text =
  Format.format
    "JSON.stringify(translator.findTerms('{}', '{}', options))"
    (textMode mode, text)

findTerms :: YomiContext -> FindTermsMode -> Text -> IO Text
findTerms ctx mode text =
  let expr = LT.unpack $ formatFindTermsQuery mode text
   in Core.jsEvalStr ctx expr

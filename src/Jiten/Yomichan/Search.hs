module Jiten.Yomichan.Search where

import Control.Monad (void)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import Formatting ((%))
import qualified Formatting
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
      stmt =
        Formatting.formatToString
          ("var options = mkOptions(" % Formatting.text % ")")
          dictsList
  void (Core.jsEvalStr ctx stmt)

findTerms :: YomiContext -> FindTermsMode -> Text -> IO Text
findTerms ctx mode text =
  let expr =
        Formatting.formatToString
          ( "translator.findTerms("
              % Formatting.stext
              % ", "
              % Formatting.stext
              % ", options)"
          )
          (textMode mode)
          text
   in Core.jsEvalStr ctx expr

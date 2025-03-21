module Jiten.Yomichan.Search where

import Control.Monad (forM, void)
import qualified Data.Aeson as A
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Data.Text.Format (Only (..))
import qualified Data.Text.Format as Format
import qualified Data.Text.Lazy as LT
import qualified Jiten.Util as Util
import Jiten.Yomichan.Core (YomiContext)
import qualified Jiten.Yomichan.Core as Core
import Jiten.Yomichan.Display (NodeBuilder)
import qualified Jiten.Yomichan.Display as Display
import Text.Blaze (Markup)

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

getAlgorithmDeinflections :: YomiContext -> Text -> IO Text
getAlgorithmDeinflections ctx text =
  let template =
        "JSON.stringify(translator._getAlgorithmDeinflections('{}', options))"
   in Core.jsEvalStr ctx (Util.strFormat template (Only text))

formatFindTermsQuery :: FindTermsMode -> Text -> LT.Text
formatFindTermsQuery mode text =
  Format.format
    "JSON.stringify(translator.findTerms('{}', '{}', options))"
    (textMode mode, text)

findTerms :: YomiContext -> FindTermsMode -> Text -> IO Text
findTerms ctx mode text =
  let expr = LT.unpack $ formatFindTermsQuery mode text
   in Core.jsEvalStr ctx expr

findTermsDOM :: YomiContext -> FindTermsMode -> Text -> IO [NodeBuilder]
findTermsDOM ctx mode text = do
  rawDOM <- Core.jsEvalStr ctx expr
  let decoded = A.decodeStrictText rawDOM
  case decoded of
    Nothing -> fail $ "failed to parse result: " <> expr
    Just nodes -> pure nodes
  where
    expr =
      let template = "JSON.stringify(findTermsDOM('{}', '{}', options))"
       in LT.unpack $ Format.format template (textMode mode, text)

findTermsHTML :: YomiContext -> FindTermsMode -> Text -> IO [Markup]
findTermsHTML ctx mode text = do
  templates <- Display.loadTemplates
  nodes <- findTermsDOM ctx mode text
  let instantiated = concatMap (Display.instantiateNodeBuilder templates) nodes
  pure (map Display.renderNode instantiated)

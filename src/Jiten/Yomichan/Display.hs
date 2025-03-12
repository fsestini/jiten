{-# LANGUAGE RecordWildCards #-}

module Jiten.Yomichan.Display where

import Control.Monad (forM, (<=<))
import Data.Aeson (Object, Value, (.:))
import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON (..), Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Format (Only (..))
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Jiten.Util as Util
import Text.Blaze (Markup)
import Text.Taggy (Element (..), Node (..), parseDOM)
import qualified Text.Taggy.Renderer

data NodeBuilder = NodeBuilder
  { nodeBuilderName :: !(Maybe Text),
    nodeBuilderDataset :: Object,
    nodeBuilderTextContent :: !(Maybe Text),
    nodeBuilderChildren :: [NodeBuilder],
    nodeBuilderQueried :: [(Text, NodeBuilder)]
  }
  deriving (Show)

instance FromJSON NodeBuilder where
  parseJSON = A.withObject "" $ \obj -> do
    name <- obj .: "name"
    dataset <- obj .: "dataset"
    textContent <- obj .: "textContent"
    children <- obj .: "children"
    queriedObjs <- obj .: "queried" :: Parser [Value]
    queried <- forM queriedObjs $ A.withObject "" $ \qObj -> do
      selector <- qObj .: "selector"
      selected <- qObj .: "selected"
      pure (selector, selected)
    pure (NodeBuilder name dataset textContent children queried)

type Templates = HashMap Text Element

loadTemplates :: IO Templates
loadTemplates = do
  file <- decodeUtf8 <$> LBS.readFile "./vendor/yomitan/ext/templates-display.html"
  let dom = parseDOM True file
  pure (HashMap.fromList (concatMap collectTemplates dom))
  where
    collectTemplates :: Node -> [(Text, Element)]
    collectTemplates (NodeElement el)
      | eltName el == "template" =
          let templateIdMay = HashMap.lookup "id" (eltAttrs el)
              templateNodeMay = listToMaybe (eltChildren el)
           in case (templateIdMay, templateNodeMay) of
                (Just templateId, Just (NodeElement templateNode)) ->
                  [(templateId, templateNode)]
                _ -> []
      | otherwise = eltChildren el >>= collectTemplates
    collectTemplates (NodeContent _) = []

applyNodeBuilder :: Templates -> NodeBuilder -> Element -> Either Text Element
applyNodeBuilder templates (NodeBuilder {..}) =
  applySelections <=< applyChildren . applyTextContent
  where
    applyChildren :: Element -> Either Text Element
    applyChildren el = do
      instantiatedChildren <-
        traverse (instantiateNodeBuilder templates) nodeBuilderChildren
      pure (el {eltChildren = eltChildren el ++ instantiatedChildren})
    applyTextContent :: Element -> Element
    applyTextContent el =
      case nodeBuilderTextContent of
        Nothing -> el
        Just text -> el {eltChildren = NodeContent text : eltChildren el}
    applySelections :: Element -> Either Text Element
    applySelections el = do
      let selections = HashMap.fromList nodeBuilderQueried
      children <- forM (eltChildren el) (modifyInnerNode selections)
      pure (el {eltChildren = children})
      where
        modifyInnerNode :: HashMap Text NodeBuilder -> Node -> Either Text Node
        modifyInnerNode selections (NodeElement innerElement) =
          let classes = case HashMap.lookup "class" (eltAttrs innerElement) of
                Just classesStr -> T.splitOn " " classesStr
                Nothing -> []
              currentSelection =
                Util.findJust
                  (\cl -> HashMap.lookup ("." <> cl) selections)
                  classes
           in case currentSelection of
                Just nb -> do
                  modifiedElement <-
                    applyNodeBuilder templates nb innerElement
                  pure (NodeElement modifiedElement)
                Nothing -> do
                  modifiedChildren <-
                    traverse
                      (modifyInnerNode selections)
                      (eltChildren innerElement)
                  pure (NodeElement (innerElement {eltChildren = modifiedChildren}))
        modifyInnerNode _ n@(NodeContent _) = pure n

instantiateNodeBuilder :: Templates -> NodeBuilder -> Either Text Node
instantiateNodeBuilder templates nb@(NodeBuilder {..}) =
  case nodeBuilderName of
    Just name
      | T.isPrefixOf "template:" name ->
          let templateName = T.drop 9 name <> "-template"
           in case HashMap.lookup templateName templates of
                Just templ ->
                  NodeElement <$> applyNodeBuilder templates nb templ
                Nothing ->
                  let msg =
                        Util.sformat
                          "template '{}' not found"
                          (Only templateName)
                   in Left msg
      | otherwise ->
          let el = Element name HashMap.empty []
           in NodeElement <$> applyNodeBuilder templates nb el
    Nothing ->
      case nodeBuilderTextContent of
        Just text -> Right (NodeContent text)
        Nothing -> Left "cannot instantiate text node without content"

renderNode :: Node -> Markup
renderNode = Text.Taggy.Renderer.toMarkup True

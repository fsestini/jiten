{-# LANGUAGE RecordWildCards #-}

module Jiten.Yomichan.Display where

import Control.Monad (forM)
import Data.Aeson (Object, Value, (.:), (.:?))
import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON (..), Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Format (Only (..))
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Jiten.Util as Util
import Text.Blaze (Markup)
import Text.Taggy (Element (..), Node (..), parseDOM)
import Text.Taggy.DOM (AttrValue)
import qualified Text.Taggy.Renderer

data NodeBuilder = NodeBuilder
  { nodeBuilderTag :: !(Maybe Text),
    nodeBuilderDataset :: Object,
    nodeBuilderTextContent :: !(Maybe Text),
    nodeBuilderChildren :: [NodeBuilder],
    nodeBuilderQueried :: [(Text, NodeBuilder)],
    nodeBuilderClassName :: !(Maybe Text),
    nodeBuilderClassList :: ![Text]
  }
  deriving (Show)

instance FromJSON NodeBuilder where
  parseJSON = A.withObject "" $ \obj -> do
    tag <- obj .: "tag"
    dataset <- obj .: "dataset"
    textContent <- obj .: "textContent"
    children <- obj .: "children"
    queriedObjs <- obj .: "queried" :: Parser [Value]
    queried <- forM queriedObjs $ A.withObject "" $ \qObj -> do
      selector <- qObj .: "selector"
      selected <- qObj .: "selected"
      pure (selector, selected)
    className <- obj .:? "className"
    classList <- obj .: "classList"
    pure
      ( NodeBuilder
          tag
          dataset
          textContent
          children
          queried
          className
          classList
      )

nbClasses :: NodeBuilder -> [Text]
nbClasses nb =
  maybe [] pure (nodeBuilderClassName nb) ++ nodeBuilderClassList nb

appendAttributes :: Text -> [Text] -> AttrValue -> AttrValue
appendAttributes separator attrs av =
  let separated = T.intercalate separator attrs
   in if T.null av then separated else av <> separator <> separated

newtype Fragment = Fragment [Node]

data Document = DocElement !Element | DocFragment ![Node]

docNodes :: Document -> [Node]
docNodes (DocElement e) = [NodeElement e]
docNodes (DocFragment es) = es

type Templates = HashMap Text Document

loadTemplates :: IO Templates
loadTemplates = do
  file <- decodeUtf8 <$> LBS.readFile "./vendor/yomitan/ext/templates-display.html"
  let dom = parseDOM True file
  pure (HashMap.fromList (concatMap collectTemplates dom))
  where
    collectTemplates :: Node -> [(Text, Document)]
    collectTemplates (NodeElement el)
      | eltName el == "template" =
          case HashMap.lookup "id" (eltAttrs el) of
            Just templateId ->
              case eltChildren el of
                [NodeElement e] -> [(templateId, DocElement e)]
                nodes -> [(templateId, (DocFragment nodes))]
            Nothing -> []
      | otherwise = eltChildren el >>= collectTemplates
    collectTemplates (NodeContent _) = []

modifyInnerNode :: Templates -> HashMap Text NodeBuilder -> Node -> Node
modifyInnerNode _ _ n@(NodeContent _) = n
modifyInnerNode templates selections (NodeElement innerElement) =
  let classes = case HashMap.lookup "class" (eltAttrs innerElement) of
        Just classesStr -> T.splitOn " " classesStr
        Nothing -> []
      currentSelection =
        Util.findJust
          (\cl -> HashMap.lookup ("." <> cl) selections)
          classes
   in case currentSelection of
        Just sel ->
          let modifiedElement = applyNodeBuilder templates sel innerElement
           in (NodeElement modifiedElement)
        Nothing ->
          let modifiedChildren =
                map
                  (modifyInnerNode templates selections)
                  (eltChildren innerElement)
           in (NodeElement (innerElement {eltChildren = modifiedChildren}))

applyNodeBuilder :: Templates -> NodeBuilder -> Element -> Element
applyNodeBuilder templates nb@(NodeBuilder {..}) =
  applyClasses . applySelections . applyChildren . applyTextContent
  where
    applyChildren :: Element -> Element
    applyChildren el =
      let instantiatedChildren =
            concatMap (instantiateNodeBuilder templates) nodeBuilderChildren
       in (el {eltChildren = eltChildren el ++ instantiatedChildren})
    applyTextContent :: Element -> Element
    applyTextContent el =
      case nodeBuilderTextContent of
        Nothing -> el
        Just text -> el {eltChildren = NodeContent text : eltChildren el}
    applySelections :: Element -> Element
    applySelections el =
      let selections = HashMap.fromList nodeBuilderQueried
          children = map (modifyInnerNode templates selections) (eltChildren el)
       in (el {eltChildren = children})
    applyClasses :: Element -> Element
    applyClasses el =
      let classes = nbClasses nb
          separated = T.intercalate " " classes
          newAttrs =
            let joinAttrs x y = mconcat [x, " ", y]
             in HashMap.insertWith joinAttrs "class" separated (eltAttrs el)
       in el {eltAttrs = newAttrs}

applyFragmentBuilder :: Templates -> NodeBuilder -> [Node] -> [Node]
applyFragmentBuilder templates (NodeBuilder {..}) =
  map (modifyInnerNode templates selections)
  where
    selections = HashMap.fromList nodeBuilderQueried

instantiateNodeBuilder :: Templates -> NodeBuilder -> [Node]
instantiateNodeBuilder templates nb@(NodeBuilder {..}) =
  case nodeBuilderTag of
    Just name
      | T.isPrefixOf "template:" name ->
          let templateName = T.drop 9 name <> "-template"
           in case HashMap.lookup templateName templates of
                Just (DocElement templ) ->
                  [NodeElement (applyNodeBuilder templates nb templ)]
                Just (DocFragment ns) -> applyFragmentBuilder templates nb ns
                Nothing ->
                  let msg =
                        Util.strFormat
                          "template '{}' not found"
                          (Only templateName)
                   in error msg
      | otherwise ->
          let el = Element name HashMap.empty []
           in [NodeElement (applyNodeBuilder templates nb el)]
    Nothing ->
      case nodeBuilderTextContent of
        Just text -> [NodeContent text]
        Nothing -> error "cannot instantiate text node without content"

renderNode :: Node -> Markup
renderNode = Text.Taggy.Renderer.toMarkup True

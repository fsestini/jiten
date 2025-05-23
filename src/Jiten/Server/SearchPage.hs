module Jiten.Server.SearchPage (instantiate) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5
  ( Html,
    body,
    dataAttribute,
    docTypeHtml,
    link,
    meta,
    toHtml,
    (!),
  )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A
import Prelude

mkParseElem :: Text -> Char -> Int -> Int -> Html
mkParseElem fullQuery c queryOffset charOffset =
  H.span
    ! A.class_ "query-parser-term"
    ! A.style sty
    ! dataAttribute "offset" (H.stringValue (show charOffset))
    ! A.onclick (H.toValue jsNavigate)
    $ toHtml c
  where
    sty =
      if queryOffset > 0 && charOffset >= queryOffset
        then "background-color: #dddddd;"
        else ""
    url = "/search?query=" <> fullQuery <> "&offset=" <> T.show charOffset
    jsNavigate = "window.location.href='" <> url <> "';"

instantiate :: [Html] -> Text -> Int -> Html
instantiate results originalQuery offset = do
  setRootAttributes . docTypeHtml $ do
    H.head $ do
      meta ! charset "UTF-8"
      meta ! name "viewport" ! content "width=device-width,initial-scale=1"
      H.title "Jiten Search"
      link ! rel "icon" ! type_ "image/png" ! href "/images/icon16.png" ! sizes "16x16"
      link ! rel "icon" ! type_ "image/png" ! href "/images/icon19.png" ! sizes "19x19"
      link ! rel "icon" ! type_ "image/png" ! href "/images/icon32.png" ! sizes "32x32"
      link ! rel "icon" ! type_ "image/png" ! href "/images/icon38.png" ! sizes "38x38"
      link ! rel "icon" ! type_ "image/png" ! href "/images/icon48.png" ! sizes "48x48"
      link ! rel "icon" ! type_ "image/png" ! href "/images/icon64.png" ! sizes "64x64"
      link ! rel "icon" ! type_ "image/png" ! href "/images/icon128.png" ! sizes "128x128"
      link ! rel "stylesheet" ! type_ "text/css" ! href "/css/material.css"
      link ! rel "stylesheet" ! type_ "text/css" ! href "/css/display.css"
      link ! rel "stylesheet" ! type_ "text/css" ! href "/css/display-pronunciation.css"
      link ! rel "stylesheet" ! type_ "text/css" ! href "/css/structured-content.css"
      link ! rel "stylesheet" ! type_ "text/css" ! href "/css/search-settings.css"
    body ! A.style "overflow: auto;" $ do
      H.div ! class_ "content-body-inner" $ do
        parsed
        H.span ! tabindex "-1" ! A.id "content-scroll-focus" $ mempty
        H.div ! A.id "dictionary-entries" $ mconcat results
  where
    setRootAttributes :: Html -> Html
    setRootAttributes h =
      foldl' (!) h $
        [ lang "en",
          dataAttribute "page-type" "search",
          dataAttribute "theme" "dark",
          dataAttribute "outer-theme" "dark",
          -- TODO: this should probably come from centralized config
          dataAttribute "average-frequency" "false",
          dataAttribute "frequency-display-mode" "split-tags-grouped"
        ]
    parsed = do
      H.form ! A.method "get" ! A.action "/search" ! class_ "search-form" $ do
        H.div ! A.style "width: 100%; padding-top: 1em;" ! class_ "search-textbox-container" $ do
          H.input
            ! A.type_ "text"
            ! A.name "query"
            ! A.id "search-textbox"
            ! A.style "width: 100%;"
            ! class_ "scrollbar"
            ! placeholder "Input a term, expression, sentence, or block of text"
            ! autocomplete "off"
            ! lang "ja"
            ! autofocus ""
            ! A.value (H.toValue originalQuery)
      H.div ! class_ "scan-disable scrollbar" ! A.id "query-parser-container" $
        H.div ! A.id "query-parser-content" ! lang "ja" $
          parseContainer
    parseContainer =
      mconcat
        . zipWith (\ix c -> mkParseElem originalQuery c offset ix) [0 ..]
        . T.unpack
        $ originalQuery

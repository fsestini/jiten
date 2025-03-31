module Jiten.Server.SearchPage (instantiate) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Jiten.Util as Util
import Text.Blaze.Html5
  ( Html,
    body,
    button,
    dataAttribute,
    docTypeHtml,
    h1,
    link,
    meta,
    toHtml,
    (!),
  )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A
import Prelude

mkParseElem :: Char -> Int -> Text -> Html
mkParseElem c offset q =
  H.a ! A.href (H.toValue url)
    $ H.span
      ! A.class_ "query-parser-term"
      ! dataAttribute "offset" (H.stringValue (show offset))
    $ toHtml c
  where
    url = "/search?query=" <> q <> "&offset=" <> T.show offset

-- TODO: take offset into account
instantiate :: [Html] -> Text -> Html
instantiate results originalQuery = do
  docTypeHtml ! lang "en" ! dataAttribute "page-type" "search" $ do
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
      link ! rel "stylesheet" ! type_ "text/css" ! href "/css/search.css"
      link ! rel "stylesheet" ! type_ "text/css" ! href "/css/search-settings.css"
    body $ do
      H.div ! class_ "content-outer" $ H.div ! class_ "content" $ do
        H.div ! class_ "content-scroll scrollbar" ! A.id "content-scroll" $ do
          H.div ! A.id "above-sticky-header" $ do
            H.div ! class_ "top-progress-bar-container" $ H.div ! class_ "progress-bar-indeterminant" ! A.id "progress-indicator" $ mempty
            H.div ! class_ "search-header-wrapper" $ H.div ! class_ "search-header" $ do
              H.div ! A.id "intro" $ h1 "Jiten Search"
          H.div ! class_ "search-header-wrapper" ! A.id "sticky-search-header" $ H.div ! class_ "search-header" $ do
            H.form ! A.method "get" ! A.action "/search" ! class_ "search-form" $ do
              H.div ! class_ "search-textbox-container" $ do
                H.input
                  ! A.type_ "text"
                  ! A.name "query"
                  ! A.id "search-textbox"
                  ! class_ "scrollbar"
                  ! placeholder "Input a term, expression, sentence, or block of text"
                  ! autocomplete "off"
                  ! lang "ja"
                  ! autofocus ""
                  ! A.value (H.toValue originalQuery)
                button
                  ! type_ "button"
                  ! A.id "clear-button"
                  ! class_ "clear-button"
                  ! onclick "window.location.href='/search';"
                  $ H.span ! class_ "icon" ! dataAttribute "icon" "cross"
                  $ mempty
                button ! type_ "submit" ! A.id "search-button" ! class_ "search-button" $
                  H.span ! class_ "icon" ! dataAttribute "icon" "magnifying-glass" $
                    mempty
              H.div ! class_ "scan-disable scrollbar" ! A.id "query-parser-container" $
                H.div ! A.id "query-parser-content" ! lang "ja" $
                  parseContainer
          H.div ! class_ "content-body" ! A.id "content-body" $
            H.div ! class_ "content-body-inner" $ do
              H.span ! tabindex "-1" ! A.id "content-scroll-focus" $ mempty
              H.div ! A.id "dictionary-entries" $ mconcat results
        H.div ! class_ "content-footer-container1" $ H.div ! class_ "content-footer-container2" $ do
          H.div ! class_ "content-footer" ! A.id "content-footer" $ mempty
          H.div ! class_ "scrollbar-spacer scrollbar" $ mempty
  where
    parseContainer =
      mconcat $ zipWith (\(ix, c) pfx -> mkParseElem c ix pfx) chars pfxs
      where
        pfxs = Util.postfixes originalQuery
        chars = zip [0 ..] (T.unpack originalQuery)

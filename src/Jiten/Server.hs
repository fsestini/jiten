module Jiten.Server where

import Network.Wai.Middleware.Cors (simpleCors)
import Text.Blaze ((!))
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (scotty)
import qualified Web.Scotty as Scotty

serve :: IO ()
serve = scotty 3000 $ do
  Scotty.middleware simpleCors
  Scotty.get "/" $ do
    Scotty.html . Blaze.renderHtml $ do
      H.h1 "Jiten"
      H.a ! A.href "/search" $ "Search"
  Scotty.get "/api/status" $ do
    Scotty.text "Ok."

runServer :: IO ()
runServer = pure ()

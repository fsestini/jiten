module Jiten.Server where

import qualified Data.Aeson as A
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream))
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket (recv, sendAll)
import Web.Scotty (scotty)
import qualified Web.Scotty as Scotty

serve :: IO ()
serve = scotty 3000 $ do
  Scotty.get "/api/status" $ do
    Scotty.text "Ok."

runServer :: IO ()
runServer = pure ()

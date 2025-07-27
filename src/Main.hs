-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE CPP               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Data.Aeson
import Miso
import Miso.Lens
import Miso.Lens.TH
import Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
data Action 
  = ServerMessage Message
  | ServerError
  | ServerClose
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data Message = Message MisoString Integer
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> Message
        <$> v .: "msg"
        <*> v .: "now"
-----------------------------------------------------------------------------
newtype Model = Model { _mMessages :: [Message] }
  deriving (Eq)
-----------------------------------------------------------------------------
makeLenses ''Model
-----------------------------------------------------------------------------
viewModel :: Model -> View Action
viewModel model =
  div_
    []
    [ h3_ [] [ text "SSE (Server-sent events) example" ]
    , p_ []
        [ text "receiving events from "
        , a_ [ href_ serverURI ] [ text serverURI ]
        , text ":"
        ]
    , ul_ [] (map fmtMessage (model ^. mMessages))
    ]
  where 
    fmtMessage (Message msg' now') = 
      li_ [] [ text (msg' <> " " <> ms (show now')) ]
-----------------------------------------------------------------------------
serverURI :: MisoString
serverURI = "https://sse.dev/test"
-----------------------------------------------------------------------------
sse :: Component Model Action
sse = (component (Model []) updateModel viewModel)
  { subs = [ sseSub serverURI handleSseMsg ]
  }
-----------------------------------------------------------------------------
handleSseMsg :: SSE Message -> Action
handleSseMsg = \case
  SSEMessage msg -> ServerMessage msg
  SSEClose -> ServerClose
  SSEError -> ServerError
-----------------------------------------------------------------------------
updateModel :: Action -> Effect Model Action
updateModel ServerClose = io_ (consoleLog "ServerClose")
updateModel ServerError = io_ (consoleLog "ServerError")
updateModel (ServerMessage msg) = do
  io_ (consoleLog "ServerMessage")
  mMessages %= (:) msg
----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run (startComponent sse)
-----------------------------------------------------------------------------

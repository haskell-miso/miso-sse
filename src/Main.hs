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
-----------------------------------------------------------------------------
data Action
  = ServerMessage Message
  | DecodeError MisoString
  | ServerError JSVal
  | ServerClose JSVal
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
viewModel :: Model -> View Model Action
viewModel model = div_ []
  [ h3_
    []
    [ text "SSE (Server-sent events) example"
    ]
  , p_
    []
    [ text "receiving events from "
    , a_ [ href_ serverURI ] [ text serverURI ]
    , text ":"
    ]
  , ul_
    []
    (fmtMessage <$> model ^. mMessages)
  ] where
      fmtMessage (msg') = li_ [] [ text $ ms (show msg') ]
-----------------------------------------------------------------------------
serverURI :: MisoString
serverURI = "https://sse.dev/test"
-----------------------------------------------------------------------------
sse :: App Model Action
sse = (component (Model []) updateModel viewModel)
  { subs =
    [ sseSub serverURI ServerMessage DecodeError ServerClose ServerError
    ]
  }
-----------------------------------------------------------------------------
updateModel :: Action -> Transition Model Action
updateModel (ServerClose o) = do
  io_ $ do
    consoleLog' o
    consoleLog "ServerClose"
updateModel (ServerError o) = do
  io_ $ do
    consoleLog "ServerError"
    consoleLog' o
updateModel (ServerMessage msg) = do
  io_ (consoleLog "ServerMessage")
  mMessages %= (++[msg])
updateModel (DecodeError msg) =
  io_ (consoleLog msg)
----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run (startApp sse)
-----------------------------------------------------------------------------

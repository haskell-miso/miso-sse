----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
module SSE (sseComponent) where
-----------------------------------------------------------------------------
import           GHC.Generics
import           Language.Javascript.JSaddle (fromJSValUnchecked)
-----------------------------------------------------------------------------
import           Miso hiding (on)
import           Miso.EventSource
import           Miso.Lens
import           Miso.String (ToMisoString)
-----------------------------------------------------------------------------
data Message
  = Message
  { dateString :: MisoString
  , message :: MisoString
  , origin :: Origin
  } deriving (Eq, Show, Generic)
-----------------------------------------------------------------------------
data Origin = CLIENT | SYSTEM | SERVER
  deriving (Eq, Show, Generic)
-----------------------------------------------------------------------------
instance ToMisoString Origin where
  toMisoString = ms . show
-----------------------------------------------------------------------------
data Action
  = OnOpen EventSource
  | OnMessage JSVal
  | OnError JSVal
  | Append Message
  | Connect
  | Disconnect
  | NoOp
  | CloseBox
  | Clear
-----------------------------------------------------------------------------
data Model = Model
  { _msg :: MisoString
  , _received :: [Message]
  , _connected :: Bool
  , _eventSource :: EventSource
  , _boxId :: Int
  } deriving Eq
-----------------------------------------------------------------------------
received :: Lens Model [Message]
received = lens _received $ \r x -> r { _received = x }
-----------------------------------------------------------------------------
connected :: Lens Model Bool
connected = lens _connected $ \r x -> r { _connected = x }
-----------------------------------------------------------------------------
eventSource :: Lens Model EventSource
eventSource = lens _eventSource $ \r x -> r { _eventSource = x }
-----------------------------------------------------------------------------
boxId :: Lens Model Int
boxId = lens _boxId $ \r x -> r { _boxId = x }
-----------------------------------------------------------------------------
emptyModel :: Int -> Model
emptyModel = Model mempty [] False emptyEventSource
-----------------------------------------------------------------------------
sseComponent :: Int -> Component parent Model Action
sseComponent box =
  (component (emptyModel box) updateModel viewModel)
    { events = defaultEvents <> keyboardEvents
    }
  where
    updateModel = \case
      Connect ->
        connect "https://sse.dev/test"
          OnOpen OnMessage OnError
      OnOpen connection -> do
        eventSource .= connection
        connected .= True
      OnMessage message ->
        io $ do
          value :: MisoString <- fromJSValUnchecked message
          date <- newDate
          dateString <- date & toLocaleString
          pure $ Append (Message dateString value SERVER)
      Append message ->
        received %= (message :)
      OnError err -> do
        connected .= False
        io_ $ do
          consoleLog "Error received"
          consoleLog' err
      NoOp ->
        pure ()
      CloseBox ->
        broadcast box
      Disconnect -> do
        connection <- use eventSource
        connection & close
        connected .= False
        io $ do
          date <- newDate
          dateString <- date & toLocaleString
          pure $ Append (Message dateString "Disconnected..." SYSTEM)
      Clear ->
        received .= []
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel m =
  div_
  [ className "sse-box" ]
  [ div_
    [ class_ "sse-header" ]
    [ div_
      []
      [ span_
        [ classList_
          [ ("sse-status", True)
          , ("status-disconnected", not (m ^. connected))
          , ("status-connected", m ^. connected)
          ]
        ]
        []
      , span_
        [ class_ "sse-id"
        ]
        [ text $ "EventSource-" <> ms (m ^. boxId) ]
      ]
    , button_
      [ aria_ "label" "Close"
      , class_ "btn-close"
      , onClick CloseBox
      ]
      [ "×" ]
    ]
    , div_
      [ class_ "sse-controls" ]
      [ optionalAttrs
        button_
        [ class_ "btn btn-success connect-btn"
        , onClick Connect
        ]
        (m ^. connected)
        [ disabled_ ]
        [ "Connect" ]
      , optionalAttrs
        button_
        [ class_ "btn btn-danger disconnect-btn"
        , onClick Disconnect
        ]
        (not (m ^. connected))
        [ disabled_ ]
        ["Disconnect"]
      , optionalAttrs
        button_
        [ class_ "btn btn-primary"
        , onClick Clear
        ]
        (null (m ^. received))
        [ disabled_ ]
        ["Clear"]
      ]
    , div_
      [ class_ "messages-list"
      ] $
      if null (m ^. received)
      then
        pure $ div_
          [ class_ "empty-state"
          ]
          [ "No messages yet"
          ]
      else messageHeader (m ^. received)
    ]
-----------------------------------------------------------------------------
messageHeader :: [Message] -> [ View model action ]
messageHeader messages = concat
  [ [ div_
      [ class_ "message-header" ]
      [ span_ [class_ "message-origin"] [ text (ms origin) ]
      , span_ [class_ "timestamp"] [ text dateString ]
      ]
    , div_ [class_ "message-content"] [ text message ]
    ]
  | Message dateString message origin <- messages
  ]
-----------------------------------------------------------------------------

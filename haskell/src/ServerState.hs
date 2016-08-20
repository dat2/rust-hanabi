{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module ServerState where

import Prelude hiding (catch)
import Control.Exception
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Aeson
import Data.DateTime
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad.Trans.Class
import Control.Monad.State
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import GameState
import Events
import MonadSupply

-- https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/exceptions-and-monad-transformers
-- show
catchStateT :: Exception e => StateT s IO a -> (e -> StateT s IO a) -> StateT s IO a
catchStateT a onE = do
    s1 <- get
    (result, s2) <- liftIO $ runStateT a s1 `catch` \e ->
        runStateT (onE e) s1
    put s2
    return result

finallyStateT :: StateT s IO a -> StateT s IO b -> StateT s IO a
finallyStateT a sequel = do
    result <- a `catchStateT` \e -> do
        _ignored <- sequel
        liftIO $ throwIO (e :: SomeException)
    _ignored <- sequel
    return result
-- /show

-- https://github.com/jaspervdj/websockets/blob/master/example/server.lhs

type Client = WS.Connection
instance Show WS.Connection where
  show _ = "Connection"

-- the player id is just for server state purposes
type PlayerId = Int
-- the server channel is just to link ids with the channel
type ServerChannel = ([PlayerId], Channel)

data ServerState =
  ServerState {
    clients :: Map.Map PlayerId Client,
    playerMap :: Map.Map PlayerId Player,
    channels :: Map.Map Text ServerChannel,
    uidSupply :: [PlayerId]
  } deriving Show

-- create a new server state object
newServerState :: ServerState
newServerState = ServerState Map.empty Map.empty Map.empty [1..]

data ThreadState =
  ThreadState {
    sharedState :: MVar ServerState,
    uid :: PlayerId,
    connection :: WS.Connection
  }

type ThreadStateMonad a = StateT ThreadState IO a

-- make it easier to write modifying functions
updateServerState :: (ServerState -> ServerState) -> ThreadStateMonad ()
updateServerState fn = do
  state <- gets sharedState

  liftIO $ modifyMVar_ state $ \s -> do
    let result = fn s
    putStrLn $ "Players: " ++ (show $ playerMap result)
    putStrLn $ "Channels: " ++ (show $ channels result)
    return result

-- update a client
updatePlayer :: Int -> (Player -> Player) -> ServerState -> ServerState
updatePlayer pid fn state@ServerState { playerMap } = state { playerMap = Map.update (Just . fn) pid playerMap }

-- add a client to the list
addClient :: Client -> (PlayerId, Player) -> ServerState -> ServerState
addClient client (uid,player) state@ServerState{ playerMap, clients }=
  let
    updateClients = Map.insert uid client
    updatePlayers = Map.insert uid player
  in
    state { clients = updateClients clients, playerMap = updatePlayers playerMap }

-- remove a client from the list
removeClient :: Client -> PlayerId -> ServerState -> ServerState
removeClient client uid state@ServerState { clients, playerMap } =
  state { clients = Map.delete uid clients, playerMap = Map.delete uid playerMap }

-- handle the create channel function
createChannel :: Text -> ServerState -> ServerState
createChannel t state@ServerState{ channels } = state { channels = Map.insert t ([], newChannel t) channels }

-- handle the join channel
joinChannel :: PlayerId -> Text -> ServerState -> ServerState
joinChannel pid ch state@ServerState{ playerMap, channels } =
  let
    maybePlayer = Map.lookup pid playerMap
    updatePlayer (player@Player { channel }) = player { channel = ch }
    addPlayerToChannel player (ids, chan@Channel { players }) = (pid:ids, chan { players = player:players })
  in
    case maybePlayer of
      Just player -> state { channels = Map.update (Just . addPlayerToChannel player) ch channels, playerMap = Map.update (Just . updatePlayer) pid playerMap }
      Nothing -> state

getChannels :: ThreadStateMonad [Channel]
getChannels = do
  st <- gets sharedState
  state <- liftIO $ readMVar st

  return . map snd . Map.elems . channels $ state

getPlayerChannel :: PlayerId -> ServerState -> Maybe Text
getPlayerChannel pid = fmap channel . Map.lookup pid . playerMap

getPlayerName :: PlayerId -> ServerState -> Maybe Text
getPlayerName pid = fmap name . Map.lookup pid . playerMap

leaveChannel :: PlayerId -> ServerState -> ServerState
leaveChannel pid state@ServerState{ playerMap, channels } =
  let
    maybePlayer = Map.lookup pid playerMap
    updatePlayer (player@Player { channel }) = player { channel = "" }
    removePlayerFromChannel player (ids, chan@Channel { players }) = (delete pid ids, chan { players = delete player players })
  in
    case maybePlayer of
      Just player -> state { channels = Map.update (Just . removePlayerFromChannel player) (channel player) channels, playerMap = Map.update (Just . updatePlayer) pid playerMap }
      Nothing -> state

sendMessage :: PlayerId -> (Text, DateTime) -> ServerState -> ServerState
sendMessage pid msg state@ServerState { playerMap, channels } =
  let
    maybePlayer = Map.lookup pid playerMap
    updateChannel (pids,chan@Channel { messages }) = (pids, chan { messages = msg:messages })
  in
    case maybePlayer of
      Just player -> state { channels = Map.update (Just . updateChannel) (channel player) channels }
      Nothing -> state

-- Broadcast sends a message to all clients
broadcastToChannel :: Text -> Event -> ThreadStateMonad ()
broadcastToChannel channel event = do
  st <- gets sharedState
  state <- liftIO $ readMVar st

  let ch = Map.lookup channel . channels $ state
  liftIO $ print ch
  -- mapMOf_ (clients.to Map.elems) (\conn -> WS.sendTextData conn message) state

-- the send function will send a WebSocketMessage
getClient :: PlayerId -> ServerState -> ThreadStateMonad Client
getClient pid s@ServerState { clients } = do
  return $ fromJust $ Map.lookup pid clients

send :: Event -> ThreadStateMonad ()
send response = do
  conn <- gets connection
  let message = (encode $ WebSocketMessage response)
  liftIO $ print message
  liftIO $ WS.sendTextData conn message

sendPlayerLeft :: ThreadStateMonad ()
sendPlayerLeft = do
  pid <- gets uid
  state <- gets sharedState
  st <- liftIO $ readMVar state
  let channel = fromJust $ getPlayerChannel pid st
  let name = fromJust $ getPlayerName pid st
  broadcastToChannel channel (PlayerLeftChannel name)

-- the respond function will respond to the decoded message
respond :: Event -> ThreadStateMonad ()
respond (SetName n) = do
  pid <- gets uid
  updateServerState $ updatePlayer pid (setName n)
respond (GetChannels) = do
  pid <- gets uid
  channels <- getChannels
  send $ (SendChannels channels)
respond (CreateChannel channel) = updateServerState $ createChannel channel

-- TODO send this channels information back to the client
respond (JoinChannel channel) = do
  pid <- gets uid
  updateServerState $ joinChannel pid channel

respond (LeaveChannel) = do
  pid <- gets uid
  updateServerState $ leaveChannel pid
  sendPlayerLeft

respond (SendMessage text) = do
  pid <- gets uid
  state <- gets sharedState
  st <- liftIO $ readMVar state
  let channel = fromJust $ getPlayerChannel pid st

  time <- liftIO $ getCurrentTime
  updateServerState $ sendMessage pid (text,time)
  broadcastToChannel channels (SendMessage text)

respond _ = do
  send $ ServerError "Not implemented"

disconnect :: ThreadStateMonad ()
disconnect = do
  pid <- gets uid
  conn <- gets connection

  sendPlayerLeft
  updateServerState $ leaveChannel pid . removeClient conn pid


clientApplication :: ThreadStateMonad ()
clientApplication = do
  -- get a new unique id for each player
  state <- gets sharedState
  conn <- gets connection

  -- get a new uid
  s <- liftIO $ readMVar state
  (uid, rest) <- runSupplyT supply (uidSupply s)
  updateServerState $ \(s@ServerState { uidSupply }) -> s { uidSupply = rest }

  let player = (uid, newPlayer)

  -- put the uid into the thread state
  modify (\s@ThreadState { uid } -> s { uid = uid })

  flip finallyStateT disconnect $ do

    -- first, add the client to the list
    updateServerState $ addClient conn player

    -- then listen for all messages and do something
    forever $ do
      -- read the message, decode, and send a response
      msg <- liftIO $ WS.receiveData conn

      -- if the event is valid, respond to it
      let maybeEvent = (decode msg :: Maybe WebSocketMessage)
      case maybeEvent of
        Just (WebSocketMessage event) -> respond event
        Nothing -> send $ ServerError "Invalid message type"

-- main Websocket application
-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.
application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  -- stateT monad
  evalStateT clientApplication (ThreadState state 0 conn)


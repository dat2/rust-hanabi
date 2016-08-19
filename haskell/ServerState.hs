{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerState where

import Data.List
import Data.Text (Text)
import Data.Aeson
import Data.DateTime
import Control.Lens
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad.Trans.Class
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import GameState
import Events
import MonadSupply

-- https://github.com/jaspervdj/websockets/blob/master/example/server.lhs

type Client = WS.Connection
instance Show WS.Connection where
  show _ = "Connection"

data ServerState =
  ServerState {
    _clients :: Map.Map PlayerId Client,
    _players :: Map.Map PlayerId Player,
    _channels :: Map.Map Text Channel,
    _uidSupply :: [PlayerId]
  } deriving Show
makeLenses ''ServerState

-- create a new server state object
newServerState :: ServerState
newServerState = ServerState Map.empty Map.empty Map.empty [1..]

newChannel :: Channel
newChannel = Channel [] []

-- make it easier to write modifying functions
updateServerState :: MVar ServerState -> (ServerState -> ServerState) -> IO ()
updateServerState state fn = modifyMVar_ state $ \s -> do
  let result = fn s
  putStrLn $ "Players: " ++ (show $ _players result)
  putStrLn $ "Channels: " ++ (show $ _channels result)
  return result

-- update a client
updatePlayer :: Int -> (Player -> Player) -> ServerState -> ServerState
updatePlayer pid fn = over (players.at pid._Just) fn

-- add a client to the list
addClient :: Client -> Player -> ServerState -> ServerState
addClient client player =
  over clients (Map.insert (_uid player) client) . over players (Map.insert (_uid player) player)

-- remove a client from the list
removeClient :: Client -> Player -> ServerState -> ServerState
removeClient client player =
  over clients (Map.delete (_uid player)) . over players (Map.delete (_uid player))

-- handle the create channel function
createChannel :: Text -> ServerState -> ServerState
createChannel t = over channels (Map.insert t newChannel)

-- handle the join channel
joinChannel :: PlayerId -> Text -> ServerState -> ServerState
joinChannel pid ch =
  over (channels.at ch._Just.playerIds) (cons pid) . set (players.at pid._Just.channel) ch

getChannels :: ServerState -> [Channel]
getChannels = view (channels.to Map.elems)

-- leaveChannel :: PlayerId -> ServerState -> ServerState
-- leaveChannel pid state =
  -- let
    -- ch = (players.at pid._Just.channel)
  -- in
    -- over (channels.at (view ch state)._Just.playerIds) (filter (/= pid)) . set ch "" $ state

-- Broadcast sends a message to all clients
-- broadcast :: Text -> ServerState -> IO ()
-- broadcast message state = do
  -- T.putStrLn message
  -- mapMOf_ (clients.to Map.elems) (\conn -> WS.sendTextData conn message) state

-- the send function will send a WebSocketMessage
getClient :: PlayerId -> MVar ServerState -> IO (Maybe Client)
getClient pid state = do
  s <- readMVar state
  return $ view (clients.at pid) s


send :: Maybe Client -> Event -> IO ()
send maybeConn response =
  case maybeConn of
    Just conn -> do
      let message = (encode $ WebSocketMessage response)
      print message
      WS.sendTextData conn message
    Nothing -> return ()

-- the respond function will respond to the decoded message
respond :: MVar ServerState -> PlayerId -> Event -> IO ()
respond state pid (SetName name) = updateServerState state $ updatePlayer pid (\c -> c { _name = name })
respond state pid (GetChannels) = do
  conn <- getClient pid state
  global <- readMVar state
  send conn $ (SendChannels . getChannels) global
respond state pid (CreateChannel channel) = updateServerState state $ createChannel channel

-- TODO send this channels information back to the client
respond state pid (JoinChannel channel) = updateServerState state $ joinChannel pid channel
-- respond state client (LeaveChannel) = updateServerState state $ leaveChannel client

respond state pid _ = do
  conn <- getClient pid state
  send conn $ ServerError "Not implemented"

-- main Websocket application
-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.
application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  -- get a new unique id for each player
  s <- readMVar state
  (uid, rest) <- runSupplyT supply (_uidSupply s)
  updateServerState state $ set uidSupply rest

  let player = Player { _name = "", _uid = uid, _channel = "" }

  -- setup the main disconnect function (it will remove the client)
  let disconnect = updateServerState state $ removeClient conn player

  flip finally disconnect $ do

    -- first, add the client to the list
    updateServerState state $ addClient conn player

    -- then listen for all messages and do something
    forever $ do
      -- read the message, decode, and send a response
      msg <- WS.receiveData conn

      -- if the event is valid, respond to it
      let maybeEvent = (decode msg :: Maybe WebSocketMessage)
      case maybeEvent of
        Just (WebSocketMessage event) -> respond state uid event
        Nothing -> send (Just conn) $ ServerError "Invalid message type"


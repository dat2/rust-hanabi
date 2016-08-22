{-# LANGUAGE OverloadedStrings #-}

module WebSocketServer where

import Control.Concurrent (MVar, readMVar, modifyMVar_)
import Control.Lens
import Control.Monad.Exception (finallyStateT)
import Control.Monad.State
import Control.Monad.Supply
import Data.Aeson
import Data.DateTime
import Data.List
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Network.WebSockets as WS

import GameTypes
import WebSocketTypes
import ServerTypes

import GameState
import ServerState


-- print something to the console
printMessage :: Show a => String -> a -> ThreadStateMonad ()
printMessage label msg = liftIO $ do
  putStrLn "=========================="
  putStrLn $ "[" ++ label ++ "]: " ++ show msg
  putStrLn "=========================="

-- get the shared state stored in the MVar
readSharedState :: ThreadStateMonad (SharedState)
readSharedState = do
  mvar <- gets _tsharedState
  state <- liftIO $ readMVar mvar
  return state

-- update the shared state stored in the MVar
updateSharedState :: (SharedState -> SharedState) -> ThreadStateMonad ()
updateSharedState fn = do
  state <- gets _tsharedState
  liftIO $ modifyMVar_ state $ \s -> return $ fn s

  -- print it after (for debug purposes)
  shared <- readSharedState
  printMessage "UPDATING SHARED STATE" (_splayers shared, _schannels shared)

-- update the thread state
updateThreadState :: (ThreadState -> ThreadState) -> ThreadStateMonad ()
updateThreadState = modify

-- broadcast a message to all players in the channel
broadcastToChannel :: Event -> ThreadStateMonad ()
broadcastToChannel event = do
  uid <- gets _tuid
  let message = encode (WebSocketMessage event)
  printMessage "SENDING" message

  state <- readSharedState
  channel <- getCurrentChannel
  let playersInChannel = state ^.schannels.at channel._Just.suids

  let connections = map snd . filter (\(u, _) -> elem u playersInChannel) . IntMap.toList . _sconnections $ state
  liftIO $ forM_ connections (flip WS.sendTextData message)

-- send a WebSocketMessage to this client
sendEvent :: Event -> ThreadStateMonad ()
sendEvent event = do
  let message = (WebSocketMessage event)
  conn <- gets _tconnection
  printMessage "SENDING" message
  liftIO $ WS.sendTextData conn (encode message)

genChannelsForClient :: ThreadStateMonad [Channel]
genChannelsForClient = do
  state <- readSharedState
  return $ getChannelsForClient state

-- get the current channel that the player is in
getCurrentChannel :: ThreadStateMonad Text
getCurrentChannel = do
  uid <- gets _tuid
  state <- readSharedState
  return $ state ^.splayers.at uid._Just.schannel

-- respond to a client request
respond :: Event -> ThreadStateMonad ()
respond (SetName new) = do
  uid <- gets _tuid
  updateSharedState $ setName uid new

respond (CreateChannel name) = do
  updateSharedState $ createChannel name

respond (GetChannels) = do
  channels <- genChannelsForClient
  sendEvent $ SendChannels channels

respond (JoinChannel name) = do
  uid <- gets _tuid
  updateSharedState $ joinChannel uid name

respond (SendMessage m) = do
  uid <- gets _tuid
  channel <- getCurrentChannel
  time <- liftIO $ getCurrentTime
  updateSharedState $ addMessageToChannel channel (uid,m,time)

  -- send the message to everybody
  state <- readSharedState
  broadcastToChannel (ServerSendMessage (state ^.splayers.at uid._Just.sname, m, DotNetTime time))

respond (LeaveChannel) = do
  uid <- gets _tuid
  channel <- getCurrentChannel
  updateSharedState $ leaveChannel uid channel

respond _ = sendEvent $ ServerError "Not Implemented"

-- remove this client from the shared state
onDisconnect :: ThreadStateMonad ()
onDisconnect = do
  uid <- gets _tuid
  channel <- getCurrentChannel
  updateSharedState $ removePlayer uid channel

createThread :: ThreadStateMonad ()
createThread = do
  -- first generate a uid for this player
  state <- readSharedState
  (uid, rest) <- runSupplyT supply $ _suidSupply state
  conn <- gets _tconnection

  -- update the supply
  -- add the thread connection to the shared state
  -- add a new player to the shared state
  updateSharedState (set suidSupply rest . addPlayer uid . addPlayerConnection uid conn)

  -- once we've generated the id, update the thread state
  updateThreadState (set tuid uid)

  -- until the player disconnects, keep responding to their messages
  flip finallyStateT onDisconnect $ do
    forever $ do
      msg <- liftIO $ WS.receiveData conn
      printMessage "RECEIVED" msg

      -- try to decode the message they sent over the websocket
      let maybeEvent = (decode msg :: Maybe WebSocketMessage)
      case maybeEvent of
        Just (WebSocketMessage event) -> respond event
        Nothing -> sendEvent $ ServerError "Invalid message received."


-- for each connection, create a new thread that has access to the shared state
application :: MVar SharedState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  evalStateT createThread (ThreadState state conn 0)

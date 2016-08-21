{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerState where

import Control.Lens
import Control.Monad.State
import Control.Concurrent (MVar, readMVar, modifyMVar_)
import Data.Aeson
import Data.DateTime
import Data.List
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Network.WebSockets as WS

import MonadSupply
import GameState
import Events
import Helpers (finallyStateT)

-- these types are all for the server to manage
type UID = Int
data SPlayer = SPlayer { _sname :: Text, _schannel :: Text } deriving Show
data SChannel = SChannel { _suids :: [UID], _smessages :: [(UID, Text, DateTime)] } deriving Show

instance Show WS.Connection where
  show _ = "Connection"

-- this state is to keep things normalized, no duplications anywhere
data SharedState =
  SharedState {
    _sconnections :: IntMap.IntMap WS.Connection,
    _splayers :: IntMap.IntMap SPlayer,
    _schannels :: Map.Map Text SChannel,
    _suidSupply :: [UID]
  } deriving Show

-- each thread has a pointer to the shared state
data ThreadState =
  ThreadState {
    _tsharedState :: MVar SharedState,
    _tconnection :: WS.Connection,
    _tuid :: UID
  }

makeLenses ''SPlayer
makeLenses ''SChannel
makeLenses ''SharedState
makeLenses ''ThreadState

type ThreadStateMonad a = StateT ThreadState IO a

-- print something to the console
printMessage :: Show a => String -> a -> ThreadStateMonad ()
printMessage label msg = liftIO $ do
  putStrLn "=========================="
  putStrLn $ "[" ++ label ++ "]: " ++ show msg
  putStrLn "=========================="

-- create a new server state object
newSharedState :: SharedState
newSharedState = SharedState IntMap.empty IntMap.empty Map.empty [1..]

-- create a new server managed player
newSPlayer :: SPlayer
newSPlayer = SPlayer { _sname = "", _schannel = "" }

-- create a new server managed channel
newSChannel :: SChannel
newSChannel = SChannel { _suids = [], _smessages = [] }

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

getChannelsForClient :: ThreadStateMonad [Channel]
getChannelsForClient = do
  state <- readSharedState

  let getPlayersForChannel chan = map (\n -> Player n []) $ state ^.. splayers.each.filtered (\p -> _schannel p == chan).sname

  let getMessagesForChannel chan = map (\(uid,text,dt) -> (state ^.splayers.at uid._Just.sname,text,DotNetTime dt)) $ chan ^.smessages

  let channels = map (\(n,chan) -> Channel n (getPlayersForChannel n) (getMessagesForChannel chan)) . Map.toList . _schannels $ state

  return $ channels

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
  updateSharedState (set (splayers.at uid._Just.sname) new)

respond (CreateChannel name) = do
  updateSharedState (over schannels (Map.insert name newSChannel))

respond (GetChannels) = do
  channels <- getChannelsForClient
  sendEvent $ SendChannels channels

respond (JoinChannel name) = do
  uid <- gets _tuid
  updateSharedState (set (splayers.at uid._Just.schannel) name . over (schannels.at name._Just.suids) (cons uid))

respond (SendMessage m) = do
  uid <- gets _tuid
  channel <- getCurrentChannel
  time <- liftIO $ getCurrentTime
  updateSharedState (over (schannels.at channel._Just.smessages) (cons (uid, m, time)))

  -- send the message to everybody
  state <- readSharedState
  broadcastToChannel (ServerSendMessage (state ^.splayers.at uid._Just.sname, m, DotNetTime time))

respond (LeaveChannel) = do
  uid <- gets _tuid
  channel <- getCurrentChannel
  updateSharedState (set (splayers.at uid._Just.schannel) "" . over (schannels.at channel._Just.suids) (delete uid))

respond _ = sendEvent $ ServerError "Not Implemented"

-- remove this client from the shared state
onDisconnect :: ThreadStateMonad ()
onDisconnect = do
  uid <- gets _tuid
  channel <- getCurrentChannel
  updateSharedState (over (schannels.at channel._Just.suids) (delete uid) . over sconnections (IntMap.delete uid) . over splayers (IntMap.delete uid))

createThread :: ThreadStateMonad ()
createThread = do
  -- first generate a uid for this player
  state <- readSharedState
  (uid, rest) <- runSupplyT supply $ _suidSupply state
  conn <- gets _tconnection

  -- update the supply
  -- add the thread connection to the shared state
  -- add a new player to the shared state
  updateSharedState (set suidSupply rest . over sconnections (IntMap.insert uid conn) . over splayers (IntMap.insert uid newSPlayer))

  -- once we've generated the id, update the thread state
  updateThreadState (set tuid uid)

  flip finallyStateT onDisconnect $ do
    forever $ do
      msg <- liftIO $ WS.receiveData conn
      printMessage "RECEIVED" msg

      let maybeEvent = (decode msg :: Maybe WebSocketMessage)
      case maybeEvent of
        Just (WebSocketMessage event) -> respond event
        Nothing -> sendEvent $ ServerError "Invalid message received."


application :: MVar SharedState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  evalStateT createThread (ThreadState state conn 0)

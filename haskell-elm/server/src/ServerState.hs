{-# LANGUAGE OverloadedStrings #-}

module ServerState where

import Control.Concurrent (MVar)
import Control.Lens
import Control.Monad.State
import Data.Aeson
import Data.DateTime
import Data.List
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Network.WebSockets as WS

import GameTypes
import ServerTypes

-- create a new server state object
newSharedState :: SharedState
newSharedState = SharedState IntMap.empty IntMap.empty Map.empty [1..]

-- create a new server managed player
newSPlayer :: SPlayer
newSPlayer = SPlayer { _sname = "", _schannel = "" }

-- create a new server managed channel
newSChannel :: SChannel
newSChannel = SChannel { _suids = [], _smessages = [] }

-- update the shared state to have a new channel
createChannel :: ChannelName -> SharedState -> SharedState
createChannel name = (over schannels (Map.insert name newSChannel))

-- update the shared state to create a new player with a new uid
addPlayer :: UID -> SharedState -> SharedState
addPlayer uid = over splayers (IntMap.insert uid newSPlayer)

-- add the connection to the map
addPlayerConnection :: UID -> WS.Connection -> SharedState -> SharedState
addPlayerConnection uid conn = over sconnections (IntMap.insert uid conn)

-- read the player (for testing)
getPlayer :: UID -> SharedState -> Maybe SPlayer
getPlayer uid state = state ^.splayers.at uid

-- update the shared state to not have the player in it
removePlayer :: UID -> ChannelName -> SharedState -> SharedState
removePlayer uid channel = (over (schannels.at channel._Just.suids) (delete uid) . over sconnections (IntMap.delete uid) . over splayers (IntMap.delete uid))

-- update the shared state to set the players name
setName :: UID -> Text -> SharedState -> SharedState
setName uid name = (set (splayers.at uid._Just.sname) name)

-- update the shared state to move the player to the channel
joinChannel :: UID -> ChannelName -> SharedState -> SharedState
joinChannel uid name = (set (splayers.at uid._Just.schannel) name . over (schannels.at name._Just.suids) (cons uid))

-- update the shared state to remove the player from the channel
leaveChannel :: UID -> ChannelName -> SharedState -> SharedState
leaveChannel uid channel = (set (splayers.at uid._Just.schannel) "" . over (schannels.at channel._Just.suids) (delete uid))

-- update the shared state to include the message
addMessageToChannel :: ChannelName -> (UID,Text,DateTime) -> SharedState -> SharedState
addMessageToChannel channel (uid,message,time) state = (over (schannels.at channel._Just.smessages) (cons (state ^.splayers.at uid._Just.sname, message, time))) state

-- generate the channels for the frontend
getChannelsForClient :: SharedState -> [Channel]
getChannelsForClient state =
  let
    getPlayersForChannel chan = map (\n -> Player n []) $ state ^.. splayers.each.filtered (\p -> _schannel p == chan).sname
    getMessagesForChannel chan = map (\(name,text,dt) -> (name,text,DotNetTime dt)) $ chan ^.smessages
  in
    map (\(n,chan) -> Channel n (getPlayersForChannel n) (getMessagesForChannel chan)) . Map.toList . _schannels $ state

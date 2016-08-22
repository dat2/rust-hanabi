{-# LANGUAGE TemplateHaskell #-}

module ServerTypes where

import Control.Concurrent (MVar)
import Control.Lens
import Control.Monad.State
import Data.DateTime
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Network.WebSockets as WS

-- these types are all for the server to manage
type UID = Int
type ChannelName = Text
data SPlayer = SPlayer { _sname :: Text, _schannel :: ChannelName } deriving (Show,Eq)
data SChannel = SChannel { _suids :: [UID], _smessages :: [(Text, Text, DateTime)] } deriving (Show,Eq)

instance Show WS.Connection where
  show _ = "Connection"

-- this state is to keep things normalized, no duplications anywhere
data SharedState =
  SharedState {
    _sconnections :: IntMap.IntMap WS.Connection,
    _splayers :: IntMap.IntMap SPlayer,
    _schannels :: Map.Map ChannelName SChannel,
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

{-# LANGUAGE TemplateHaskell #-}

module Events where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B

import GameState

data Event =
    SetName Text
  | GetChannels
  | CreateChannel Text
  | JoinChannel Text
  | LeaveChannel
  | PlayerLeftChannel Text
  | SendMessage Text
  | StartGame
  | SendChannels [Channel]
  | ServerError Text
  deriving (Eq, Show)

data WebSocketMessage =
  WebSocketMessage {
    payload :: Event
  } deriving (Show)

deriveJSON defaultOptions{ sumEncoding = ObjectWithSingleField } ''Event
deriveJSON defaultOptions{ sumEncoding = ObjectWithSingleField } ''WebSocketMessage

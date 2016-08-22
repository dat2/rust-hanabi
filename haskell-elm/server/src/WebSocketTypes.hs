{-# LANGUAGE TemplateHaskell #-}

module WebSocketTypes where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B

import GameTypes

data Event =
    SetName Text
  | GetChannels
  | CreateChannel Text
  | JoinChannel Text
  | LeaveChannel
  | SendMessage Text
  | PlayerLeftChannel Text
  | StartGame
  -- send back to client
  | SendChannels [Channel]
  | ServerSendMessage (Text,Text,DotNetTime)
  | ServerError Text
  deriving (Eq, Show)

data WebSocketMessage =
  WebSocketMessage {
    payload :: Event
  } deriving (Show)

deriveJSON defaultOptions{ sumEncoding = ObjectWithSingleField } ''Event
deriveJSON defaultOptions{ sumEncoding = ObjectWithSingleField } ''WebSocketMessage

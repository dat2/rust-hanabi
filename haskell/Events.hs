{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Events where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B

data Event =
    SetName String
  | GetChannels
  | CreateChannel String
  | JoinChannel String
  | LeaveChannel
  | StartGame
  | SendMessage String
  | SendChannels [String]
  -- | SendState(GameState),
  | EventError String
  deriving (Eq, Show)

data WebSocketMessage =
  WebSocketMessage {
    payload :: Event
  } deriving (Show)

deriveJSON defaultOptions{ sumEncoding = ObjectWithSingleField } ''Event
deriveJSON defaultOptions{ sumEncoding = ObjectWithSingleField } ''WebSocketMessage

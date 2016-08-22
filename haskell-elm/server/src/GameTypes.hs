{-# LANGUAGE TemplateHaskell #-}

module GameTypes where

import System.Time
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, Value(Number),DotNetTime)
import Data.Text (Text)
import Elm.Derive

data Colour =
    Yellow
  | Red
  | Blue
  | Green
  | White
  deriving (Show,Eq)

data Card =
  Card {
    number :: Int,
    colour :: Colour
  } deriving (Show,Eq)

data Player =
  Player {
    name :: Text,
    cards :: [Card]
  } deriving (Show,Eq)

data Channel =
  Channel {
    cname :: Text,
    players :: [Player],
    messages :: [(Text, Text, DotNetTime)]
  } deriving (Show,Eq)

deriveBoth defaultOptions ''Colour
deriveBoth defaultOptions{ sumEncoding = ObjectWithSingleField } ''Card
deriveBoth defaultOptions{ sumEncoding = ObjectWithSingleField } ''Player
deriveBoth defaultOptions{ sumEncoding = ObjectWithSingleField } ''Channel

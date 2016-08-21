{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Elm.Derive

import System.Random
import System.Random.Shuffle
import System.Time
import Control.Applicative
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, Value(Number),DotNetTime)
import Data.DateTime
import Data.Maybe
import Data.Ratio
import Data.Text (Text)

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

-- generate a list of cards
generateDeck :: [Card]
generateDeck = map (\(n, c) -> Card n c) [(i,c) | i <- [1..5], c <- [Yellow, Red, Blue, Green, White]]

-- re order deck
shuffleDeck :: [Card] -> [Card]
shuffleDeck cs = shuffle' cs (length cs) (mkStdGen 0)

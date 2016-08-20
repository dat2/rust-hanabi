{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Elm.Derive

import System.Random
import System.Random.Shuffle
-- import Data.Aeson
-- import Data.Aeson.TH
import Data.Text (Text)
import Data.DateTime

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
    cards :: [Card],
    channel :: Text
  } deriving (Show,Eq)

newPlayer :: Player
newPlayer = Player { name = "", cards = [], channel = "" }

setName :: Text -> Player -> Player
setName n player = player { name = n }

data Channel =
  Channel {
    cname :: Text,
    players :: [Player],
    messages :: [(Text, DateTime)]
  } deriving (Show,Eq)

newChannel :: Text -> Channel
newChannel name = Channel { players = [], messages = [], cname = name }

deriveBoth defaultOptions ''Colour
deriveBoth defaultOptions{ sumEncoding = ObjectWithSingleField } ''Card
-- remove the underscore
deriveBoth defaultOptions{ sumEncoding = ObjectWithSingleField } ''Player
deriveBoth defaultOptions{ sumEncoding = ObjectWithSingleField } ''Channel

-- generate a list of cards
generateDeck :: [Card]
generateDeck = map (\(n, c) -> Card n c) [(i,c) | i <- [1..5], c <- [Yellow, Red, Blue, Green, White]]

-- re order deck
shuffleDeck :: [Card] -> [Card]
shuffleDeck cs = shuffle' cs (length cs) (mkStdGen 0)

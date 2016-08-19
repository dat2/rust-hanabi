{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState where

import System.Random
import System.Random.Shuffle
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
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

type PlayerId = Int
data Player =
  Player {
    _name :: Text,
    _uid :: PlayerId,
    _channel :: Text
  } deriving (Show, Eq)

data Channel =
  Channel {
    _playerIds :: [Int],
    _messages :: [(Text,DateTime)]
  } deriving (Eq, Show)

makeLenses ''Player
makeLenses ''Channel

deriveJSON defaultOptions ''Colour
deriveJSON defaultOptions{ sumEncoding = ObjectWithSingleField } ''Card

-- generate a list of cards
generateDeck :: [Card]
generateDeck = map (\(n, c) -> Card n c) [(i,c) | i <- [1..5], c <- [Yellow, Red, Blue, Green, White]]

-- re order deck
shuffleDeck :: [Card] -> [Card]
shuffleDeck cs = shuffle' cs (length cs) (mkStdGen 0)

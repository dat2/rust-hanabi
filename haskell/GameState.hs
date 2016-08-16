{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module GameState where

import GHC.Generics
import System.Random
import System.Random.Shuffle
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Colour =
    Yellow
  | Red
  | Blue
  | Green
  | White
  deriving (Show, Generic)

instance ToJSON Colour where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Colour

data Card =
  Card {
    number :: Int,
    colour :: Colour
  } deriving (Show, Generic)

instance ToJSON Card where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Card

-- generate a list of cards
generateDeck :: [Card]
generateDeck = map (\(n, c) -> Card n c) [(i,c) | i <- [1..5], c <- [Yellow, Red, Blue, Green, White]]

-- re order deck
shuffleDeck :: [Card] -> [Card]
shuffleDeck cs = shuffle' cs (length cs) (mkStdGen 0)

{-# LANGUAGE OverloadedStrings #-}
module GameState where

import System.Random
import System.Random.Shuffle

import GameTypes

-- generate a list of cards
generateDeck :: [Card]
generateDeck = map (\(n, c) -> Card n c) [(i,c) | i <- [1..5], c <- [Yellow, Red, Blue, Green, White]]

-- re order deck
shuffleDeck :: [Card] -> [Card]
shuffleDeck cs = shuffle' cs (length cs) (mkStdGen 0)

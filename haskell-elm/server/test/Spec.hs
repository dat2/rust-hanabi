{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck

import Data.Maybe
import ServerTypes
import ServerState

main :: IO ()
main = hspec $ do
  describe "ServerTypes.SPlayer" $ do
    it "addPlayer should create a new empty player" $ do
      let state = newSharedState
      let afterState = addPlayer 1 state
      getPlayer 1 afterState `shouldBe` Just newSPlayer

    it "setName should update the name correctly" $ do
      let state = newSharedState
      let afterState = setName 1 "Nick" . addPlayer 1 $ state
      let player = fromJust $ getPlayer 1 afterState
      _sname player `shouldBe` "Nick"

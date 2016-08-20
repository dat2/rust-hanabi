{-# LANGUAGE NamedFieldPuns #-}
module Main where

import System.Environment
import Control.Concurrent (newMVar)
import Configuration.Dotenv
import qualified Configuration.Dotenv as Dotenv
import qualified Network.WebSockets as WS

import Events
import GameState
import ServerState

main :: IO ()
main = do

  -- load dotenv file
  Dotenv.loadFile False ".env"
  bind <- getEnv "BIND"
  port <- getEnv "PORT"

  putStrLn $ "Listening on " ++ bind  ++ ":" ++ port

  state <- newMVar newServerState
  WS.runServer bind (read port :: Int) $ application state

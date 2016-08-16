{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Data.Text (Text)
import Data.Aeson
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Configuration.Dotenv
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Configuration.Dotenv as Dotenv

import GameState
import Events

-- https://github.com/jaspervdj/websockets/blob/master/example/server.lhs

data Player =
  Player {
    name :: Text
  } deriving (Show, Eq)

type Client = (Player, WS.Connection)
data ServerState =
  ServerState {
    clients :: [Client]
  }

-- create a new server state object
newServerState :: ServerState
newServerState = ServerState []

-- get the number of clients
numClients :: ServerState -> Int
numClients = length . clients

-- add a client to the list
addClient :: Client -> ServerState -> ServerState
addClient client state = state { clients = client : (clients state) }

-- remove a client from the list
removeClient :: Client -> ServerState -> ServerState
removeClient client state = state { clients = filter ((/= fst client) . fst) $ clients state }

-- Broadcast sends a message to all clients
broadcast :: Text -> ServerState -> IO ()
broadcast message state = do
  T.putStrLn message
  forM_ (clients state) $ \(_, conn) -> WS.sendTextData conn message

-- main Websocket application
-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.
application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  forever $ do
    msg <- WS.receiveData conn
    clients <- readMVar state
    -- print $ msg
    print $ (decode msg :: Maybe WebSocketMessage)


main :: IO ()
main = do
  -- load dotenv file
  Dotenv.loadFile False ".env"
  bind <- getEnv "BIND"
  port <- getEnv "PORT"

  putStrLn $ "Listening on " ++ bind  ++ ":" ++ port

  state <- newMVar newServerState
  WS.runServer bind (read port :: Int) $ application state

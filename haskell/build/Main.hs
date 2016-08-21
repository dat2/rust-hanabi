module Main where

import System.Directory
import System.FilePath
import Data.Proxy
import Data.String.Utils
import Elm.Module
import GameState

locationToSaveIn :: FilePath
locationToSaveIn = "elm-client" </> "src" </> "GameState.elm"

prepare :: String -> String
prepare m =
  let
    replaceWhere = replace "module GameState where" "module GameState exposing (..)\n\nimport Date exposing (Date,Day)\nimport DecodeDate exposing (jsonDecDate,jsonEncDate)"
    replaceDateTime = replace "DotNetTime" "Date"
  in
    (replaceWhere . replaceDateTime) m

main :: IO ()
main = do
  let m = makeElmModule "GameState" [ DefineElm (Proxy :: Proxy Colour), DefineElm (Proxy :: Proxy Card), DefineElm (Proxy :: Proxy Player), DefineElm (Proxy :: Proxy Channel) ]

  path <- getCurrentDirectory

  writeFile (path </> locationToSaveIn) (prepare m)

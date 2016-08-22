module Main where

import System.Directory
import System.FilePath
import Data.Proxy
import Data.String.Utils
import Elm.Module

import GameTypes

locationToSaveIn :: FilePath
locationToSaveIn = "client" </> "src" </> "GameTypes.elm"

prepare :: String -> String
prepare m =
  let
    replaceWhere = replace "module GameTypes where" "module GameTypes exposing (..)\n\nimport Date exposing (Date,Day)\nimport DecodeDate exposing (jsonDecDate,jsonEncDate)"
    replaceDateTime = replace "DotNetTime" "Date"
  in
    (replaceWhere . replaceDateTime) m

main :: IO ()
main = do
  let m = makeElmModule "GameTypes" [ DefineElm (Proxy :: Proxy Colour), DefineElm (Proxy :: Proxy Card), DefineElm (Proxy :: Proxy Player), DefineElm (Proxy :: Proxy Channel) ]

  path <- getCurrentDirectory

  putStrLn $ "Writing the generated elm file to " ++ (takeDirectory path </> locationToSaveIn)
  writeFile (takeDirectory path </> locationToSaveIn) (prepare m)

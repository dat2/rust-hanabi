module GameTypes exposing (..)

import Date exposing (Date,Day)
import DecodeDate exposing (jsonDecDate,jsonEncDate)

import Json.Decode
import Json.Decode exposing ((:=))
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict
import Set


type Colour  =
    Yellow 
    | Red 
    | Blue 
    | Green 
    | White 

jsonDecColour : Json.Decode.Decoder ( Colour )
jsonDecColour = decodeSumUnaries "Colour" jsonDecDictColour
jsonDecDictColour = Dict.fromList [("Yellow", Yellow), ("Red", Red), ("Blue", Blue), ("Green", Green), ("White", White)]

jsonEncColour : Colour -> Value
jsonEncColour  val =
    case val of
        Yellow -> Json.Encode.string "Yellow"
        Red -> Json.Encode.string "Red"
        Blue -> Json.Encode.string "Blue"
        Green -> Json.Encode.string "Green"
        White -> Json.Encode.string "White"



type alias Card  =
   { number: Int
   , colour: Colour
   }

jsonDecCard : Json.Decode.Decoder ( Card )
jsonDecCard =
   ("number" := Json.Decode.int) `Json.Decode.andThen` \pnumber ->
   ("colour" := jsonDecColour) `Json.Decode.andThen` \pcolour ->
   Json.Decode.succeed {number = pnumber, colour = pcolour}

jsonEncCard : Card -> Value
jsonEncCard  val =
   Json.Encode.object
   [ ("number", Json.Encode.int val.number)
   , ("colour", jsonEncColour val.colour)
   ]



type alias Player  =
   { name: String
   , cards: (List Card)
   }

jsonDecPlayer : Json.Decode.Decoder ( Player )
jsonDecPlayer =
   ("name" := Json.Decode.string) `Json.Decode.andThen` \pname ->
   ("cards" := Json.Decode.list (jsonDecCard)) `Json.Decode.andThen` \pcards ->
   Json.Decode.succeed {name = pname, cards = pcards}

jsonEncPlayer : Player -> Value
jsonEncPlayer  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("cards", (Json.Encode.list << List.map jsonEncCard) val.cards)
   ]



type alias Channel  =
   { cname: String
   , players: (List Player)
   , messages: (List (String, String, Date))
   }

jsonDecChannel : Json.Decode.Decoder ( Channel )
jsonDecChannel =
   ("cname" := Json.Decode.string) `Json.Decode.andThen` \pcname ->
   ("players" := Json.Decode.list (jsonDecPlayer)) `Json.Decode.andThen` \pplayers ->
   ("messages" := Json.Decode.list (Json.Decode.tuple3 (,,) (Json.Decode.string) (Json.Decode.string) (jsonDecDate))) `Json.Decode.andThen` \pmessages ->
   Json.Decode.succeed {cname = pcname, players = pplayers, messages = pmessages}

jsonEncChannel : Channel -> Value
jsonEncChannel  val =
   Json.Encode.object
   [ ("cname", Json.Encode.string val.cname)
   , ("players", (Json.Encode.list << List.map jsonEncPlayer) val.players)
   , ("messages", (Json.Encode.list << List.map (\(v1,v2,v3) -> Json.Encode.list [(Json.Encode.string) v1,(Json.Encode.string) v2,(jsonEncDate) v3])) val.messages)
   ]


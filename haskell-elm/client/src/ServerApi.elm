module ServerApi exposing (ClientEvent(..), encodeClientEvent, ServerEvent(..), decodeResponse)

import Json.Decode exposing (..)
import Json.Encode as E
import String
import Date exposing (Date)

import GameTypes exposing (..)
import DecodeDate exposing (jsonDecDate)

type ClientEvent
  = SetName String
  | GetChannels
  | CreateChannel String
  | JoinChannel String
  | LeaveChannel
  | SendMessage String

-- this function will take the ClientEvent and automatically generate a string
-- representing the form of json that the back end is expecting:
-- { payload: { [event]: messageData } }
encodeClientEvent : ClientEvent -> String
encodeClientEvent msg =
  let
    (event, data) =
      case msg of
        SetName name -> ("SetName", E.string name)
        GetChannels -> ("GetChannels", E.list [])
        CreateChannel channel -> ("CreateChannel", E.string channel)
        JoinChannel channel -> ("JoinChannel", E.string channel)
        LeaveChannel -> ("LeaveChannel", E.list [])
        SendMessage message -> ("SendMessage", E.string message)
  in
    E.encode 0 (E.object [("payload", E.object [(event, data)] )])

type ServerEvent
  = SendChannels (List Channel)
  | ServerSendMessage (String, String, Date)
  | ServerError String

serverEvent : Decoder ServerEvent
serverEvent =
  "payload" := oneOf
    [ "SendChannels" := object1 SendChannels (list jsonDecChannel)
    , "ServerSendMessage" := tuple3 (\player -> \message -> \d -> ServerSendMessage (player, message, d)) string string jsonDecDate
    , "Error" := object1 ServerError string
    ]

decodeResponse : String -> Result String ServerEvent
decodeResponse = decodeString serverEvent

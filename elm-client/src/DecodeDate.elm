module DecodeDate exposing (..)

import Json.Decode exposing (..)
import Json.Encode as E
import Date
import String

jsonDecDate : Decoder Date.Date
jsonDecDate =
  string `andThen` \val ->
    case String.toFloat val of
      Err err -> fail err
      Ok ms -> succeed <| Date.fromTime ms

jsonEncDate : Date.Date -> E.Value
jsonEncDate date = E.float (Date.toTime date)

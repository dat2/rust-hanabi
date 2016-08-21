module DecodeDate exposing (..)

import Debug
import List
import Json.Decode exposing (..)
import Json.Encode as E
import Date
import String
import Parser exposing (Parser,parse,empty,token,(*>),(<*))
import Parser.Number exposing (integer)

dropEnd : Int -> List a -> List a
dropEnd n l = List.reverse (List.drop n (List.reverse l))

time : Parser Int
time = (token "/Date(") *> integer <* (token ")/")

jsonDecDate : Decoder Date.Date
jsonDecDate =
  string `andThen` \val ->
    case parse time val of
      Err err -> fail ( Debug.log "failed to decode date " err )
      Ok ms -> succeed <| Date.fromTime (toFloat ms)

jsonEncDate : Date.Date -> E.Value
jsonEncDate date = E.string ("/Date(" ++ toString (Date.toTime date) ++ "/")

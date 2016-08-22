port module Main exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (type', placeholder, class, value)
import Html.Events exposing (onInput, onClick, onSubmit)

import WebSocket
import Task
import Http
import List
import Maybe
import Debug
import String

import Navigation
import Hop exposing (makeUrl, makeUrlFromLocation, matchUrl, setQuery)
import Hop.Types exposing (Config, Query, Location, PathMatcher, Router)
import Hop.Matchers exposing (..)

import ServerApi exposing (..)
import GameTypes exposing (..)

-- model
type alias Model =
  { webSocketAddress : String
  , welcomePagePlayerName : String
  , lobbyPageChannels : List Channel
  , lobbyPageChannelName : String
  , lobbyPageCreateModalOpen : Bool
  , gamePageMessage : String
  , channel : Channel
  , error : String
  , location : Location
  , route : Route
  }

-- routing
type Route
  = WelcomeRoute
  | LobbyRoute
  | GameRoute
  | NotFoundRoute

matchers : List (PathMatcher Route)
matchers =
  [ match1 WelcomeRoute ""
  , match1 LobbyRoute "/lobby"
  , match1 GameRoute "/game"
  ]

routerConfig : Config Route
routerConfig =
  { hash = True
  , basePath = ""
  , matchers = matchers
  , notFound = NotFoundRoute
  }

urlParser : Navigation.Parser (Route, Location)
urlParser =
  Navigation.makeParser (.href >> matchUrl routerConfig)

urlUpdate : (Route, Location) -> Model -> (Model, Cmd Msg)
urlUpdate (route, location) model =
  ({ model | route = route, location = location }, Cmd.none)

-- commands so we can batch them
navigateTo : String -> Cmd Msg
navigateTo path = makeUrl routerConfig path |> Navigation.newUrl

sendEvent : String -> ClientEvent -> Cmd Msg
sendEvent webSocketAddress event = WebSocket.send webSocketAddress (encodeClientEvent event)

-- update
type Msg
  -- the base messages
  = NavigateTo String
  | Send ClientEvent
  | Receive (Result String ServerEvent)
  -- the welcome page messages
  | ChangePlayerName String
  | SubmitName
  -- the lobby page messages
  | SetModalOpen Bool
  | ChangeChannelName String
  | SubmitChannelName
  | JoinThisChannel String
  -- the game page messages
  | ChangeMessageText String
  | LeaveThisChannel
  | SubmitMessage

updateWithServerEvent : Result String ServerEvent -> Model -> Model
updateWithServerEvent event model =
  let
    channel = model.channel
    updateModel ev =
      case ev of
        SendChannels lobbyPageChannels -> { model | lobbyPageChannels = lobbyPageChannels }
        ServerSendMessage msg -> { model | channel = { channel | messages = msg :: model.channel.messages } }
        ServerError error -> { model | error = error }
  in
    case event of
      Ok ev -> updateModel ev
      Err error -> Debug.log "ServerEvent error" { model | error = error }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- the base events (navigate, send, receive)
    NavigateTo path -> (model, navigateTo path)
    Send event -> (model, sendEvent model.webSocketAddress event)
    Receive event -> (updateWithServerEvent event model, Cmd.none)
    -- the welcome page events
    ChangePlayerName new -> ({ model | welcomePagePlayerName = new }, Cmd.none)
    SubmitName -> (model, Cmd.batch [  navigateTo "/lobby", sendEvent model.webSocketAddress (SetName model.welcomePagePlayerName) ])
    -- the lobby page events
    SetModalOpen open -> ({ model | lobbyPageCreateModalOpen = open, lobbyPageChannelName = if open then model.lobbyPageChannelName else "" }, Cmd.none)
    ChangeChannelName new -> ({ model | lobbyPageChannelName = new }, Cmd.none)
    SubmitChannelName -> ({ model | lobbyPageCreateModalOpen = False, lobbyPageChannelName = "" }, Cmd.batch [sendEvent model.webSocketAddress GetChannels, sendEvent model.webSocketAddress (CreateChannel model.lobbyPageChannelName)])
    JoinThisChannel channel -> ({ model | channel = Maybe.withDefault (model.channel) (List.head (List.filter (\c -> c.cname == channel) model.lobbyPageChannels)) }, Cmd.batch [ sendEvent model.webSocketAddress (JoinChannel channel), navigateTo "/game" ])
    -- the game page messages
    ChangeMessageText text -> ({ model | gamePageMessage = text }, Cmd.none)
    SubmitMessage -> ({ model | gamePageMessage = "" },sendEvent model.webSocketAddress (SendMessage model.gamePageMessage))
    LeaveThisChannel -> (model, Cmd.batch [ sendEvent model.webSocketAddress LeaveChannel, navigateTo "/lobby" ])

-- view
makeIcon : String -> Html Msg
makeIcon icon =
  span [class "icon"]
  [
    i [class ("fa fa-" ++ icon)] []
  ]

makeIconButton : List (Attribute Msg) -> String -> String -> String -> Html Msg
makeIconButton attributes icon extraClasses t =
  button (List.append [class ("button " ++ extraClasses), type' "submit"] attributes)
  [
    makeIcon icon
  , span [] [text t]
  ]

-- the welcome page is the first page you see
welcomePage : Model -> Html Msg
welcomePage model =
  section [class "hero is-primary is-fullheight"]
    [ div [class "hero-body"]
      [ div [class "container"]
        [ h1 [class "title"] [text "Welcome to Hanabi!"]
        , form [onSubmit SubmitName]
            [ label [class "label"] [text "Type in your name"]
            , p [class "control"]
              [ input [class "input", type' "text", placeholder "Type in your name", onInput ChangePlayerName] [] ]
            , makeIconButton [] "play" "is-success" "Let's Go!"
            ]
        ]
      ]
    ]

createChannelModal : Model -> Html Msg
createChannelModal model =
  div [class ("modal" ++ (if model.lobbyPageCreateModalOpen then " is-active" else "") )]
    [ div [class "modal-background", onClick (SetModalOpen False)] []
    , div [class "modal-card"]
        [ header [class "modal-card-head"]
            [ p [class "modal-card-title"] [text "Create Channel"]
            , button [class "delete", onClick (SetModalOpen False)] []
            ]
        , section [class "modal-card-body"]
            [ form [ onSubmit SubmitChannelName ]
                [ p [class "control"]
                    [ label [class "control"] [text "Channel Name"]
                    , input [class "input", type' "text", onInput ChangeChannelName, value model.lobbyPageChannelName] []
                    ]
                ]
            ]
        , footer [class "modal-card-foot"]
            [ a [class "button is-primary", onClick SubmitChannelName] [text "Create Channel"]
            , a [class "button", onClick (SetModalOpen False)] [text "Cancel"]
            ]
        ]
    ]

channelCard : Channel -> Html Msg
channelCard channel =
  div [class "column is-one-quarter"]
    [ div [class "card is-fullwidth"]
      [ div [class "card-header"]
          [ p [class "card-header-title"] [text channel.cname]
          , a [class "card-header-icon"] [ i [class "fa fa-sign-in", onClick (JoinThisChannel channel.cname)] [] ]
          ]
      , div [class "card-content"]
          [ div [class "content"]
              [ text "Players in the channel ("
              , text (toString (List.length channel.players))
              , text ")"
              ]
          ]
      , div [class "card-footer"]
          [ a [class "card-footer-item",onClick (JoinThisChannel channel.cname)]
            [ makeIcon "sign-in"
            , span [] [text "Join"]
            ]
          ]
      ]
    ]

lobbyPageChannelsList : Model -> Html Msg
lobbyPageChannelsList model =
  div [class "columns is-multiline"]
    (List.map channelCard model.lobbyPageChannels)

-- the lobby page is where you can create a channel, and join lobbyPageChannels
lobbyPage : Model -> Html Msg
lobbyPage model =
  div []
    [ nav [class "nav has-shadow"]
      [ div [class "nav-left"]
        [ div [class "nav-item"] [ makeIconButton [ onClick (Send (GetChannels))] "refresh" "" "Reload Channels" ]
        , div [class "nav-item"] [ makeIconButton [ onClick (SetModalOpen True)] "plus-circle" "" "Create new channel" ]
        ]
      ]
    , div [class "section"]
      [ createChannelModal model
      , lobbyPageChannelsList model
      ]
    ]


formatDate : Date -> String
formatDate d = (String.padLeft 2 '0' (toString (Date.hour d))) ++ ":" ++ (String.padLeft 2 '0' (toString (Date.minute d))) ++ ":" ++ (String.padLeft 2 '0' (toString (Date.second d)))

gamePageMessageBox : (String, String, Date) -> Html Msg
gamePageMessageBox (player, message, date) =
  div [class "box"]
    [ article [class "media"]
        [ div [class "media-content"]
            [ p []
              [ strong [] [text player]
              , text " "
              , small [] [text (formatDate date)]
              , br [] []
              , text message
              ]
            ]
        ]
    ]

gamePage : Model -> Html Msg
gamePage model =
  div []
    [ nav [class "nav has-shadow"]
      [ div [class "nav-left"]
        [ div [class "nav-item"] [ makeIconButton [ onClick LeaveThisChannel ] "backward" "" "Go Back" ] ] ]
    , div [class "columns"]
      [ div [class "column is-three-quarters"] []
      , div [class "column"]
          [ text "chat messages"
          , section [class "section"]
              [ form [onSubmit SubmitMessage]
                [ input [class "input", type' "text", onInput ChangeMessageText, value model.gamePageMessage] [] ]
              ]
          , div [class "content"] (List.map gamePageMessageBox model.channel.messages)
          ]
      ]
    ]

pageView : Model -> Html Msg
pageView model =
  case model.route of
    WelcomeRoute -> welcomePage model
    LobbyRoute -> lobbyPage model
    GameRoute -> gamePage model
    NotFoundRoute -> div [] [ h2 [class "title"] [text "How did you get here??"] ]

view : Model -> Html Msg
view model =
  div [] [ pageView model ]

-- subscriptions
port webSocketAddress : (String -> msg) -> Sub msg

decodeResponseToMsg : String -> Msg
decodeResponseToMsg s = Receive (decodeResponse s)

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen model.webSocketAddress decodeResponseToMsg

newChannel : String -> Channel
newChannel name = { players = [], messages = [], cname = name }

-- init
type alias Flags =
  { webSocketAddress : String
  }

init : Flags -> (Route, Hop.Types.Location) -> (Model, Cmd Msg)
init flags (route, location) =
  ({ webSocketAddress = flags.webSocketAddress
    , welcomePagePlayerName = ""
    , lobbyPageChannels = []
    , lobbyPageChannelName = ""
    , lobbyPageCreateModalOpen = False
    , gamePageMessage = ""
    , channel = newChannel ""
    , error = ""
    , location = location
    , route = route
  }
  , sendEvent flags.webSocketAddress GetChannels
  )

-- finally wire it all together
main =
  Navigation.programWithFlags urlParser
    { init = init
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = subscriptions
    , view = view
  }

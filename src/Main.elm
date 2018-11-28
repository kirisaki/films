module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Browser.Events
import Markdown.Block as Md
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Url exposing (Url)
import Json.Decode as Decode

type alias Model =
    { position : Int
    , title : String
    , slides : List (Html Msg)
    }

init : String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init src url navKey =
    ( { position = 1
      , title = ""
      , slides = []
      }
    , Cmd.none
    )

type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | Next
    | Prev
    | NoOp
      

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUrl url ->
            ( model
            , Cmd.none
            )

        ClickedLink req ->
            ( model
            , Cmd.none
            )

        Next ->
            ( { model | position = model.position + 1 }
            , Cmd.none
            )

        Prev ->
            ( { model | position = model.position - 1 }
            , Cmd.none
            )
        NoOp ->
            ( model
            , Cmd.none
            )

keyHandler : Sub Msg
keyHandler =
    let
        toMsg str =
            case Debug.log "input: " str of
                "ArrowLeft" -> Prev
                "ArrowRight" -> Next
                " " -> Next
                _ -> NoOp
        decoder = Decode.map toMsg (Decode.field "key" Decode.string)
    in
        Browser.Events.onKeyDown decoder
            
subscriptions : Model -> Sub Msg
subscriptions  _ =
    keyHandler

document : Model -> Browser.Document Msg
document model =
    { title = model.title
    , body = view model
    }

view : Model -> List (Html Msg)
view model = 
    [ div [ class "container"
          ]
          [ h1 [] [ text (String.fromInt model.position) ]
          ]
    ]

main : Program String Model Msg
main =
    Browser.application
        { init = init
        , view = document
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }

module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Markdown.Block as Md
import Html exposing (..)
import Html.Events exposing (..)
import Url exposing (Url)

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
    | MovePage Int 


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "message: " msg of
        ChangedUrl url ->
            ( model
            , Cmd.none
            )

        ClickedLink req ->
            ( model
            , Cmd.none
            )

        MovePage n ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


document : Model -> Browser.Document Msg
document model =
    { title = model.title
    , body = view model
    }

view : Model -> List (Html Msg)
view model = 
    [ h1 [] [ text (String.fromInt model.position) ]
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

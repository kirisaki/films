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
    ( { position = 0
      , title = ""
      , slides = toSlides src
      }
    , Cmd.none
    )

toSlides : String -> List (Html Msg)
toSlides src =
    Md.parse Nothing src
        |> List.filter ((/=) Md.ThematicBreak)
        |> List.map ( Md.toHtml >> div [ class "slide" ] )
    
type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | Next
    | Prev
    | NoOp
      
movePosition : Model -> Int -> Model
movePosition model dest =
    let
        max = List.length model.slides - 1
        min = 0
        n = dest
    in
        if min <= n && n <= max
        then { model | position = n }
        else model
      
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
            ( movePosition model (model.position + 1)
            , Cmd.none
            )

        Prev ->
            ( movePosition model (model.position - 1)
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
            case str of
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

module Main exposing (..)

import Array exposing (Array, fromList, toList)
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Markdown.Block as Md
import Url exposing (Url)


type alias Model =
    { position : Int
    , title : String
    , slides : Array (Html Msg)
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    ( { position = 0
      , title = ""
      , slides = toSlides """
# aaa

---

# bbb

## aaa

---

## ccc
- 1
- 2
- 3
"""
      }
    , Cmd.none
   )

split : (a -> Bool) -> Array a -> Array (Array a)
split p xs =
    let
        indices =
            Array.toIndexedList xs
                |> List.filter (Tuple.second >> p)
                |> List.map Tuple.first
        starts = 0 :: List.map ((+) 1) indices
        ends = indices ++ [Array.length xs]
        zip a b =
            case (a, b) of
                ([], _) -> []
                (_, []) -> []
                (x :: xs_, y :: ys_) ->
                    (x, y) :: zip xs_ ys_
        ranges = zip starts ends
    in
        fromList <| List.map (\(n, m) -> Array.slice n m xs) ranges

            
toSlides : String -> Array (Html Msg)
toSlides src =
    Md.parse Nothing src
        |> fromList
        |> split ((==) Md.ThematicBreak)
        |> Array.map (toList >> List.map Md.toHtml >> List.concat  >> div [ class "slide" ])


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | Next
    | Prev
    | NoOp


movePosition : Model -> Int -> Model
movePosition model dest =
    let
        max =
            Array.length model.slides - 1

        min =
            0

        n =
            dest
    in
    if min <= n && n <= max then
        { model | position = n }

    else
        model


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
                "ArrowLeft" ->
                    Prev

                "ArrowRight" ->
                    Next

                " " ->
                    Next

                _ ->
                    NoOp

        decoder =
            Decode.map toMsg (Decode.field "key" Decode.string)
    in
    Browser.Events.onKeyDown decoder


subscriptions : Model -> Sub Msg
subscriptions _ =
    keyHandler


document : Model -> Browser.Document Msg
document model =
    { title = model.title
    , body = view model
    }


view : Model -> List (Html Msg)
view model =
    let
        slide = case Array.get model.position model.slides of
                    Just v -> [v]
                    Nothing -> []
    in
        [ div
          [ class "container"
          ] slide
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = document
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }

module Main exposing (..)

import Array exposing (Array, fromList, toList, empty)
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Markdown.Block as Md
import Markdown.Inline
import Url exposing (Url)
import SyntaxHighlight exposing (useTheme, monokai, elm, toBlockHtml)

type alias Slide =
    { title : String
    , slide : Html Msg
    }


type alias Model =
    { position : Int
    , slides : Array Slide
    }


init : String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init loc url navKey =
    ( { position = 0
      , slides = empty
      }
    , Http.get
        { url = loc
        , expect = Http.expectString ReceiveMarkdown
        }
   )


split : (a -> Bool) -> Array a -> Array (Array a)
split p xs =
    let
        indices =
            Array.toIndexedList xs
                |> List.filter (Tuple.second >> p)
                |> List.map Tuple.first

        starts =
            0 :: List.map ((+) 1) indices

        ends =
            indices ++ [ Array.length xs ]

        ranges =
            zip starts ends
    in
    fromList <| List.map (\( n, m ) -> Array.slice n m xs) ranges


zip : List a -> List b -> List ( a, b )
zip a b =
    case ( a, b ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( x :: xs, y :: ys ) ->
            ( x, y ) :: zip xs ys


consArray : a -> Array a -> Array a
consArray x ys =
    Array.append (fromList [ x ]) ys


headArray : Array a -> Maybe a
headArray =
    Array.get 0


tailArray : Array a -> Array a
tailArray xs =
    Array.slice 1 (Array.length xs) xs


zipArray : Array a -> Array b -> Array ( a, b )
zipArray =
    zipArrayWith Tuple.pair


zipArrayWith : (a -> b -> c) -> Array a -> Array b -> Array c
zipArrayWith f a b =
    case ( headArray a, headArray b ) of
        ( Nothing, _ ) ->
            empty

        ( _, Nothing ) ->
            empty

        ( Just x, Just y ) ->
            let
                xs =
                    tailArray a

                ys =
                    tailArray b
            in
            consArray (f x y) (zipArrayWith f xs ys)

customHtmlBlock : Md.Block b i -> List (Html msg)
customHtmlBlock block =
    case block of
        Md.CodeBlock codeType blocks ->
            [text <| Debug.toString codeType]

        _ ->
            Md.defaultHtml
                (Just customHtmlBlock)
                (Just (Markdown.Inline.toHtml))
                block
                    
toSlides : String -> Array Slide
toSlides src =
    let
        blocks =
            Md.parse Nothing src
                |> fromList
                |> split ((==) Md.ThematicBreak)

        findHeading b =
            case headArray b of
                Nothing ->
                    Nothing

                Just (Md.Heading str 1 _) ->
                    Just str

                _ ->
                    findHeading (tailArray b)

        headings =
            Array.map findHeading blocks

        padding tb hs =
            case headArray hs of
                Nothing ->
                    empty

                Just Nothing ->
                    consArray tb (padding tb (tailArray hs))

                Just (Just t) ->
                    consArray t (padding t (tailArray hs))

        titles =
            padding "" headings

        contents =
            Array.map (toList >> List.map customHtmlBlock >> List.concat >> div [ class "slide" ]) blocks
    in
    zipArrayWith Slide titles contents


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | Next
    | Prev
    | ReceiveMarkdown (Result Http.Error String)
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
            
        ReceiveMarkdown (Ok str) ->
            ( { model | slides = toSlides str }
            , Cmd.none
            )

        ReceiveMarkdown (Err e) ->
            let
                _ = Debug.log "http error: " e
            in
                ( model
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
    { title =
        case Array.get model.position model.slides of
            Just s ->
                s.title

            Nothing ->
                ""
    , body = view model
    }


view : Model -> List (Html Msg)
view model =
    let
        slide =
            case Array.get model.position model.slides of
                Just v ->
                    [ v.slide ]

                Nothing ->
                    []
    in
    [ div
        [ class "container"
        ]
        slide
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

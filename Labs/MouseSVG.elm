module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Mouse exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { x : Int
    , y : Int
    }


initialModel : Model
initialModel =
    { x = 0
    , y = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = Position Int Int


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        Position x y ->
            ( { model | x = x, y = y }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.moves (\{ x, y } -> Position x y)



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div
        [ style
            [ ( "background-color", "Blue" )
            , ( "width", "180px" )
            , ( "height", "80px" )
            , ( "border-radius", "50%" )
            , ( "position", "absolute" )
            , ( "left", toString model.x ++ "px" )
            , ( "top", toString model.y ++ "px" )
            , ( "color", "white" )
            , ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            ]
        ]
        [ Html.text "Move Me" ]

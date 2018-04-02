module EscapeLavaMaze exposing (..)

import AnimationFrame as Anim
import Char exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Keyboard exposing (..)
import List as List
import Mouse exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- MODEL


type Outcome
    = Playing


type alias Model =
    { x : Int, y : Int, p : Outcome, level : Int }


type Msg
    = KeyMsg Int
    | ResetMsg


type alias Rect =
    { x : Int, y : Int, w : Int, h : Int, state : Bool }



-- STYLE


menuStyle =
    Attr.style
        [ ( "text-align", "center" )
        , ( "width", "100%" )
        , ( "height", "100%" )
        , ( "background-image", "url(https://media.giphy.com/media/3ov9jYd9chmwCNQl8c/giphy.gif)" )
        , ( "padding-bottom", "21%" )
        , ( "margin-top", "-7.5%" )
        , ( "opacity", "0.9" )
        , ( "background-repeat", "no-repeat" )
        , ( "background-size", "cover" )
        , ( "font-family", "Lucida Console" )
        ]


buttonstyle =
    Attr.style
        [ ( "background-color", "black" )
        , ( "color", "white" )
        , ( "border", "2px solid black" )
        , ( "width", "250px" )
        , ( "height", "50px" )
        , ( "opacity", "0.8" )
        , ( "margin-top", "4%" )
        , ( "font-size", "20px" )
        ]


descriptionStyle =
    Attr.style
        [ ( "color", "white" )
        , ( "font-size", "50px" )
        , ( "text-align", "center" )
        , ( "opacity", "1" )
        , ( "margin-top", "50px" )
        ]


titlebarStyle =
    Attr.style
        [ ( "color", "white" )
        , ( "font-size", "70px" )
        , ( "text-align", "center" )
        , ( "padding-top", "10%" )
        ]


divStyle =
    Attr.style [ ( "height", "95%" ), ( "width", "95%" ) ]


mainMenu : Model -> Html.Html Msg
mainMenu model =
    div [ menuStyle ]
        [ p [ titlebarStyle ] [ Html.text "Ready to Escape the maze?" ]
        , p [ descriptionStyle ] [ Html.text "Objective: Escape The Lava Maze" ]
        , p [ descriptionStyle ] [ Html.text "Press the arrow keys to move" ]
        , p [ descriptionStyle ] [ Html.text "The wall might suck you in!" ]
        , p [ descriptionStyle ] [ Html.text "Some walls are invisible." ]
        , button [ buttonstyle, onClick ResetMsg ] [ Html.text "Let's Play" ]
        ]


winscreenStyle =
    Attr.style
        [ ( "text-align", "center" )
        , ( "width", "100%" )
        , ( "height", "100%" )
        , ( "background-image", "url(https://media.giphy.com/media/7SsXQOg7WKCl2/giphy.gif)" )
        , ( "padding-bottom", "20%" )
        , ( "opacity", "0.9" )
        , ( "background-repeat", "no-repeat" )
        , ( "background-size", "cover" )
        , ( "padding-top", "50px" )
        ]


winScreen : Model -> Html.Html Msg
winScreen model =
    div [ winscreenStyle ]
        [ p [ descriptionStyle ] [ Html.text "Congratulations!!!" ]
        , p [ descriptionStyle ] [ Html.text "You've successfully crossed the maze" ]
        , button [ buttonstyle, onClick ResetMsg ] [ Html.text "Play Again" ]
        ]



-- LEVELS


level1View : Model -> Html.Html Msg
level1View model =
    let
        posX =
            toString model.x

        posY =
            toString model.y
    in
    div [ Attr.align "center" ]
        [ svg [ width "1200", height "650", Attr.align "center", lavalStyle ]
            [ Svg.circle [ cx posX, cy posY, r "30", fill "sandybrown" ] []
            , Svg.image [ x "0", y "600", width "1200", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "0", y "450", width "1100", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "100", y "300", width "1100", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "0", y "150", width "1100", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "100", y "0", width "1100", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            ]
        ]


lavalStyle =
    Attr.style
        [ ( "align", "center" )
        , ( "background-image", "url(http://mauipride.org/wp-content/uploads/2015/03/Seamless-Lava-Crack-Background-1024x640.jpg)" )
        , ( "border-style", "solid solid solid solid" )
        , ( "border-color", "grey" )
        , ( "margin-top", "8%" )
        , ( "background-repeat", "no-repeat" )
        , ( "background-size", "cover" )
        ]


svgBall =
    Attr.style
        [ ( "background", "url(https://s3-us-west-2.amazonaws.com/s.cdpn.io/18515/grumpy.jpeg)" )
        , ( "background-size", "cover" )
        ]


level2View : Model -> Html.Html Msg
level2View model =
    let
        posX =
            toString model.x

        posY =
            toString model.y
    in
    div [ Attr.align "center" ]
        [ svg [ width "1200", height "650", Attr.align "center", lavalStyle ]
            [ Svg.circle [ cx posX, cy posY, r "30", fill "sandybrown" ] []
            , Svg.image [ x "0", y "600", width "580", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "670", y "600", width "580", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "0", y "450", width "1100", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "100", y "300", width "100", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "700", y "300", width "400", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "300", y "300", width "200", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "0", y "150", width "100", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "350", y "150", width "500", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "100", y "0", width "400", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            , Svg.image [ x "800", y "0", width "400", height "50px", Svg.Attributes.preserveAspectRatio "none", Svg.Attributes.xlinkHref "https://media.giphy.com/media/l378sOvgHwmw7G21q/giphy.gif" ] []
            ]
        ]


levelRects : Model -> List Rect
levelRects model =
    case model.level of
        1 ->
            [ { x = 0, y = 600, w = 1200, h = 50, state = False }
            , { x = 0, y = 450, w = 1100, h = 50, state = False }
            , { x = 100, y = 300, w = 1100, h = 50, state = False }
            , { x = 0, y = 150, w = 1100, h = 50, state = False }
            , { x = 100, y = 0, w = 1100, h = 50, state = False }
            ]

        2 ->
            [ { x = 0, y = 600, w = 1200, h = 50, state = False }
            , { x = 0, y = 450, w = 1100, h = 50, state = False }
            , { x = 100, y = 300, w = 1100, h = 50, state = False }
            , { x = 0, y = 150, w = 1100, h = 50, state = False }
            , { x = 100, y = 0, w = 1100, h = 50, state = False }
            , { x = 800, y = 0, w = 400, h = 50, state = False }
            ]

        _ ->
            []


init =
    ( { x = 625, y = 550, p = Playing, level = 0 }, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    if model.p == Playing then
        case model.level of
            0 ->
                mainMenu model

            1 ->
                level1View model

            2 ->
                level2View model

            3 ->
                winScreen model

            _ ->
                mainMenu model
    else
        mainMenu model



--RECT LOGIC


defaultState =
    True


valInList : List a -> a -> Int -> Bool
valInList level defaultState pos =
    case level of
        [] ->
            False

        x :: xs ->
            if x == defaultState then
                True
            else
                valInList xs defaultState (pos + 1)


inRectsArea : List Rect -> Model -> Bool
inRectsArea level model =
    valInList (List.map (inRect model) level) defaultState 0


inRect : Model -> Rect -> Bool
inRect model rec =
    (model.x >= (rec.x - 30)) && (model.x <= (rec.x + rec.w) + 30) && (model.y <= (rec.y + rec.h) + 30) && (model.y >= rec.y - 30)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg state ->
            keyMsgUpdate state model

        ResetMsg ->
            ( { x = 625, y = 550, p = Playing, level = 1 }, Cmd.none )


keyMsgUpdate : Int -> Model -> ( Model, Cmd Msg )
keyMsgUpdate keyCode model =
    if model.y < -18 then
        ( { x = 625, y = 550, p = Playing, level = model.level + 1 }, Cmd.none )
    else if model.x <= 25 then
        ( { x = model.x + 10, y = model.y, p = Playing, level = model.level }, Cmd.none )
    else if model.y >= 570 then
        ( { x = model.x, y = model.y - 10, p = Playing, level = model.level }, Cmd.none )
    else if model.x >= 1170 then
        ( { x = model.x - 10, y = model.y, p = Playing, level = model.level }, Cmd.none )
    else if inRectsArea (levelRects model) model then
        ( { x = model.x, y = model.y + 60, p = Playing, level = model.level }, Cmd.none )
    else
        case keyCode of
            40 ->
                ( { x = model.x, y = model.y + 30, p = Playing, level = model.level }, Cmd.none )

            --up
            38 ->
                ( { x = model.x, y = model.y - 30, p = Playing, level = model.level }, Cmd.none )

            --down
            39 ->
                ( { x = model.x + 30, y = model.y, p = Playing, level = model.level }, Cmd.none )

            --left
            37 ->
                ( { x = model.x - 30, y = model.y, p = Playing, level = model.level }, Cmd.none )

            --right
            _ ->
                ( model, Cmd.none )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyMsg



----------------------------------------------------------------------


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

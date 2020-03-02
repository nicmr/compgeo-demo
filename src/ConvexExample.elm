module ConvexExample exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, div, text, button, input, p)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class)
import Html.Attributes as Attributes

import Canvas
import Canvas.Settings
import Color

import Random

import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Convex


type alias Point = (Float, Float)


type Msg
    = RandomPoints (List Point) | NewPoints | DrawHull | UpdatePointCount String




type alias Model =
    { count : Float
    , points : List (Float, Float)
    , draw_hull : Bool
    , pointCount : Int
    }

init : Model
init = 
    { count = 0
    , points = [(200, 350), (100, 250), (300, 50)]
    , draw_hull = False
    , pointCount = 10
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RandomPoints points ->
            ( { model | points = points, draw_hull = False}, Cmd.none )
        NewPoints ->
            (model, Random.generate RandomPoints (randomPointList model.pointCount))
        DrawHull ->
            ({model | draw_hull = True}, Cmd.none)
        UpdatePointCount newCount ->
            ({model | pointCount = String.toInt newCount |> Maybe.withDefault 10 }, Cmd.none)

-- view and view constants 

width: Int
width = 400
height: Int
height = 400

view : Model -> Html Msg
view model =
    div
    [ class "convex-columns"
    ]
    [ div
        [ class "convex-ui"
        ]
        [ p [] [text ( String.fromInt model.pointCount ++ " Points" ) ]
        , input
            [ Attributes.type_ "range"
            , Attributes.min "0"
            , Attributes.max "50"
            , Attributes.value <| String.fromInt model.pointCount
            , onInput UpdatePointCount
            ] []
        , button [onClick NewPoints] [text "Generate new points"]
        , button [onClick DrawHull] [text "Draw convex hull"]
        ]
    , div
        [ class "convex-visualization"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ class "convex-canvas"]
            [ clearScreen
            , render model.points model.draw_hull
            ]
        ]
    ]


-- canvas render functions and helpers

render: List Point -> Bool -> Canvas.Renderable
render points draw_hull =
    let
        circles = List.map (\(x,y) -> Canvas.circle (x,y) (toFloat width / 150) ) points
    in
        if draw_hull then
            List.map (\(x,y) -> vec2 x y) points
            |> Convex.convexHull
            |> vecsToPath
            |> (\maybeHull ->
                case maybeHull of
                    Just hull -> hull :: circles
                    Nothing -> circles
            )
            |> Canvas.shapes [Canvas.Settings.fill (Color.rgb255 168 95 117 )]
            -- |> Canvas.shapes [Canvas.Settings.fill (Color.hsl (200/360) 0.6 0.65)]
                -- #A85F75
        else
            Canvas.shapes [] circles


vecsToPath: List Math.Vector2.Vec2 -> Maybe Canvas.Shape
vecsToPath vecs =
    case vecs of
        [] -> Nothing
        [x] -> Just <| Canvas.path (getX x, getY x) []
        (x::xs) -> Just <| Canvas.path (getX x, getY x) <| List.map(\a -> Canvas.lineTo (getX a, getY a)) xs

-- clears the canvas by coloring everything white
clearScreen : Canvas.Renderable
clearScreen =
    Canvas.shapes [ Canvas.Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) (toFloat width) (toFloat height) ]


-- random point generators

randomPoint: Random.Generator Point
randomPoint =
    let
        max_w = toFloat (width-1)
        max_h = toFloat (height-1)
    in
      Random.map2(\x y -> (x, y)) (Random.float 1 max_w) (Random.float 1 max_h)


randomPointList : Int -> Random.Generator (List Point)
randomPointList length =
    Random.list length randomPoint
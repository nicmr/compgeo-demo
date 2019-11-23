module ConvexExample exposing (Model, Msg(..), init, update, render)

import Random
import Canvas
import Canvas.Settings
import Color

import Math.Vector2 exposing (Vec2, vec2, getX, getY)


import Convex


type alias Point = (Float, Float)


type Msg
    = RandomPoints (List Point) | NewPoints | DrawHull


width = 400
height = 400

type alias Model =
    { count : Float
    , points : List (Float, Float)
    , draw_hull : Bool
    }


init = 
    { count = 0
    , points = [(230, 350)]
    , draw_hull = False
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RandomPoints points ->
            ( { model | points = points, draw_hull = False}, Cmd.none )
        NewPoints ->
            (model, Random.generate RandomPoints (randomPointList 10))
        DrawHull ->
            ({model | draw_hull = True}, Cmd.none)


-- random point generators

randomPoint: Random.Generator Point
randomPoint =
    let
        max_w = width-1
        max_h = height-1
    in
      Random.map2(\x y -> (x, y)) (Random.float 1 max_w) (Random.float 1 max_h)


randomPointList : Int -> Random.Generator (List Point)
randomPointList length =
    Random.list length randomPoint


-- canvas render functions and helpers

render: List Point -> Bool -> Canvas.Renderable
render points draw_hull =
    let
        size = width / 3
        circles = List.map (\(x,y) -> Canvas.circle (x,y) (size/50) ) points
    in
        case draw_hull of
            False ->
                Canvas.shapes [] circles
            True ->
                List.map (\(x,y) -> vec2 x y) points
                |> Convex.convexHull
                |> vecsToPath
                |> (\maybeHull ->
                    case maybeHull of
                        Just hull -> hull :: circles
                        Nothing -> circles
                )
                |> Canvas.shapes [Canvas.Settings.fill (Color.hsl (200/360) 0.6 0.65)]


vecsToPath: List Math.Vector2.Vec2 -> Maybe Canvas.Shape
vecsToPath vecs =
    case vecs of
        [] -> Nothing
        [x] -> Just <| Canvas.path (getX x, getY x) []
        (x::xs) -> Just <| Canvas.path (getX x, getY x) <| List.map(\a -> Canvas.lineTo (getX a, getY a)) xs

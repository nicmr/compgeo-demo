module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Convex

import Random


type alias Point = (Float, Float)


type alias Model =
    { count : Float
    , points : List (Float, Float)
    , draw_hull : Bool
    }


type Msg
    = Frame Float | RandomPoints (List Point) | NewPoints | DrawHull


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( { count = 0, points = [(2.3, 4.5)], draw_hull = False }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> onAnimationFrameDelta Frame
        }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Frame _ ->
            ( { model | count = model.count + 1 }, Cmd.none )
        RandomPoints points ->
            ( { model | points = points, draw_hull = False}, Cmd.none )
        NewPoints ->
            (model, Random.generate RandomPoints (randomPointList 10))
        DrawHull ->
            ({model | draw_hull = True}, Cmd.none)


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


width = 400
height = 400


centerX = width / 2
centerY = height / 2


view : Model -> Html Msg
view model =
    div [ style "display" "flex"
        , style "justify-content" "center"
        , style "flex-direction" "row"
        ]
        [ div
            [style "display" "flex"
            -- , style "justify-content" "flex-start"
            , style "flex-direction" "column"
            ]
            [ Html.button [onClick NewPoints] [text "Generate new points"]
            , Html.button [onClick DrawHull] [text "Draw convex hull"]
            ]
        , div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "flex-start"
            ]
            [ Canvas.toHtml
                ( width, height )
                [ style "border" "10px solid rgba(0,0,0,0.1)" ]
                [ clearScreen
                , renderConvexExample model.points model.draw_hull
                ]
            ]
        -- , div
        --     [ style "display" "flex"
        --     , style "justify-content" "center"
        --     , style "align-items" "flex-start"
        --     ]
        --     [ Canvas.toHtml
        --         ( width, height )
        --         [ style "border" "10px solid rgba(0,0,0,0.1)" ]
        --         [ clearScreen
        --         , render model.count
        --         ]
        --     ]
        ]
    

clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


renderConvexExample: List Point -> Bool -> Canvas.Renderable
renderConvexExample points draw_hull =
    let
        size = width / 3
        circles = List.map (\(x,y) -> Canvas.circle (x,y) (size/50) ) points
    in
        case draw_hull of
            False ->
                shapes [] circles
            True ->
                List.map (\(x,y) -> vec2 x y) points
                |> Convex.convexHull
                |> vecsToPath
                |> (\maybeHull ->
                    case maybeHull of
                        Just hull -> hull :: circles
                        Nothing -> circles
                )
                |> shapes [fill (Color.hsl (200/360) 0.6 0.65)]


vecsToPath: List Math.Vector2.Vec2 -> Maybe Canvas.Shape
vecsToPath vecs =
    case vecs of
        [] -> Nothing
        [x] -> Just <| Canvas.path (getX x, getY x) []
        (x::xs) -> Just <| Canvas.path (getX x, getY x) <| List.map(\a -> Canvas.lineTo (getX a, getY a)) xs


render count =
    let
        size = width / 3
        x = -(size / 2)
        y = -(size / 2)
        rotation = degrees (count * 3)
        hue =
            toFloat (count / 4 |> floor |> modBy 100) / 100
    in
        shapes
            [ transform
                [ translate centerX centerY
                , rotate rotation
                ]
            , fill (Color.hsl hue 0.3 0.7)
            ]
            [ rect ( x, y ) size size ]
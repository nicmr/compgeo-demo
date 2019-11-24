module Main exposing (main)

import Browser
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import ConvexExample

import Random


type alias Point = (Float, Float)


type Example = ExConvex | ExTriangulation


type alias Model =
    { convexModel : ConvexExample.Model
    , active: Example
    }


type Msg
    = ConvexMsg ConvexExample.Msg | SwitchTo Example


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


init : () -> (Model, Cmd Msg)
init _ =
    ( { convexModel = ConvexExample.init , active = ExConvex}
    , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ConvexMsg cmsg ->
            let
                (newConvexModel, convexCmd) = ConvexExample.update cmsg model.convexModel
            in
                ({model | convexModel = newConvexModel}, Cmd.map ConvexMsg convexCmd)

        SwitchTo example ->
            ({ model | active = example}, Cmd.none)


-- view and view constants

width = 400
height = 400


view : Model -> Html Msg
view model =
    let
        activeView =
            case model.active of
                ExConvex ->
                    [ div
                        [style "display" "flex"
                        , style "justify-content" "flex-start"
                        , style "flex-direction" "column"
                        ]
                        [ Html.button [onClick (ConvexMsg ConvexExample.NewPoints)] [text "Generate new points"]
                        , Html.button [onClick (ConvexMsg ConvexExample.DrawHull)] [text "Draw convex hull"]
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
                            , ConvexExample.render model.convexModel.points model.convexModel.draw_hull
                            ]
                        ]
                    ]
                ExTriangulation -> []
    in
        div [ style "display" "flex"
            , style "justify-content" "center"
            , style "flex-direction" "row"
            ]
            
            (List.append activeView
                [ div []
                    [ Html.button [onClick (SwitchTo ExTriangulation)] [text "Switch to ExTriangulation example"]
                    , Html.button [onClick (SwitchTo ExConvex)] [text "Switch to ExConvex example"]
                    ]
                ]
            )
    

-- canvas render functions and helpers

clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]
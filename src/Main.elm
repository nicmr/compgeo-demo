module Main exposing (main)

import Browser
import Html exposing (Html, div, text, h1, nav, p)
import Html.Events exposing (onClick)
-- import Html.Attributes exposing (style)
import Html.Attributes exposing (class)


import ConvexExample


type alias Point = (Float, Float)


type Example = ExConvex | ExTriangulation


type alias Model =
    { convexModel : ConvexExample.Model
    , active: Example
    }


type Msg
    = ConvexMsg ConvexExample.Msg | SwitchTo Example

mapConvexMsg : ConvexExample.Msg -> Msg
mapConvexMsg msg =
    ConvexMsg msg


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
                ExConvex -> ConvexExample.view model.convexModel |> Html.map mapConvexMsg
                    
                ExTriangulation -> div [] []
        -- add a css class depending on whether the element is actively selected or not
        (convexStateClass, triangulationStateClass) =
            case model.active of
                ExConvex -> (class "active", class "inactive")
                ExTriangulation -> (class "inactive", class "active")
               
    in  div []
        [ div [ class "header" ]
            [ div [ class "title-box" ]
                [ h1 [] [ text "Compgeo-demo" ]
                ]
            , nav [ class "nav" ]
                [ div [ class "nav-elem", convexStateClass, onClick (SwitchTo ExConvex)] [ text "Convex example" ]
                , div [ class "nav-elem", triangulationStateClass, onClick (SwitchTo ExTriangulation)] [ text "Triangulation example" ]
                ]
            ]
        , div [ class "content-columns"]
            [ activeView ]
        ]
       
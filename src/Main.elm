module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (class, style)
import AnimationFrame
import Time exposing (Time)
import Animation exposing (..)
import Ease


type alias Model =
    { clock : Time
    , player : Animation
    }


type Msg
    = NoOp
    | Frame Time


main : Program Never Model Msg
main =
    Html.program { view = view, init = init, update = update, subscriptions = subscriptions }


init : ( Model, Cmd Msg )
init =
    { clock = 0, player = playerAnimation } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Frame
          -- , Keyboard.downs KeyDown
          -- , Keyboard.ups KeyUp
        ]


playerAnimation =
    animation 0
        |> from 1
        |> to 6
        |> duration 1000
        |> ease Ease.linear


handleFrame : Model -> Time -> ( Model, Cmd Msg )
handleFrame model t =
    let
        clock =
            model.clock + t

        player =
            if isDone clock model.player then
                undo clock model.player
            else
                model.player
    in
        { clock = clock, player = player } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame t ->
            handleFrame model t

        NoOp ->
            model ! []


tile : String -> Html Msg
tile path =
    let
        sprite =
            "http://localhost:8001/src/sprites/" ++ path

        styles =
            style [ ( "background-image", "url('" ++ sprite ++ "')" ) ]
    in
        div [ class "tile", styles ] []


emptyTile =
    div [ class "tile" ] []


layerFloor =
    div [ class "layer" ]
        [ tile "TileSet/floor01.png"
        , tile "TileSet/obstacle02.png"
        , tile "TileSet/floor01.png"
        , row
        , tile "TileSet/obstacle03.png"
        , tile "TileSet/floor01.png"
        , tile "TileSet/obstacle03.png"
        , row
        , tile "TileSet/floor01.png"
        , tile "TileSet/obstacle02.png"
        , tile "TileSet/floor01.png"
        ]


layerItems { clock, player } =
    let
        value =
            round <| animate clock player
    in
        div [ class "layer" ]
            [ emptyTile
            , tile <| "Enemy02Idle/0" ++ (toString value) ++ ".png"
            , emptyTile
            , row
            , emptyTile
            , tile <| "PlayerIdle/0" ++ (toString value) ++ ".png"
            , tile <| "Enemy01Idle/0" ++ (toString value) ++ ".png"
            , row
            , emptyTile
            , emptyTile
            , emptyTile
            ]


row =
    div [ class "row" ] []


render : Model -> Html Msg
render model =
    div [ class "grid" ]
        [ layerFloor
        , layerItems model
        ]


view : Model -> Html Msg
view model =
    render model

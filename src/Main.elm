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


type Facing
    = Right
    | Left


type TileType
    = Player
    | Enemy1
    | Enemy2


type Tile
    = Tile { type_ : TileType, defaultFace : Facing }


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


tile : Tile -> Int -> Html Msg
tile (Tile { type_ }) step =
    let
        path =
            tilePath type_ step

        sprite =
            "http://localhost:8001/src/sprites/" ++ path

        facing =
            Right

        styles =
            style
                [ ( "background-image", "url('" ++ sprite ++ "')" )
                , ( "transform", tileTransform facing )
                ]
    in
        div [ class "tile", styles ] []


tileRaw : String -> List ( String, String ) -> Html Msg
tileRaw path attributes =
    let
        sprite =
            "http://localhost:8001/src/sprites/" ++ path

        styles =
            style <|
                [ ( "background-image", "url('" ++ sprite ++ "')" )
                ]
                    ++ attributes
    in
        div [ class "tile", styles ] []


player : Tile
player =
    Tile { type_ = Player, defaultFace = Left }


enemy1 : Tile
enemy1 =
    Tile { type_ = Enemy1, defaultFace = Right }


enemy2 : Tile
enemy2 =
    Tile { type_ = Enemy2, defaultFace = Right }


tileTransform facing =
    let
        scaleX =
            case facing of
                Right ->
                    1

                Left ->
                    -1
    in
        "scaleX(" ++ (toString scaleX) ++ ")"


tilePath : TileType -> Int -> String
tilePath type_ step =
    let
        folder =
            case type_ of
                Player ->
                    "PlayerIdle"

                Enemy1 ->
                    "Enemy01Idle"

                Enemy2 ->
                    "Enemy02Idle"
    in
        folder ++ "/0" ++ (toString step) ++ ".png"


emptyTile =
    div [ class "tile" ] []


layerFloor =
    div [ class "layer" ]
        [ tileRaw "TileSet/floor01.png" []
        , tileRaw "TileSet/obstacle02.png" []
        , tileRaw "TileSet/floor01.png" []
        , row
        , tileRaw "TileSet/obstacle03.png" []
        , tileRaw "TileSet/floor01.png" []
        , tileRaw "TileSet/obstacle03.png" []
        , row
        , tileRaw "TileSet/floor01.png" []
        , tileRaw "TileSet/obstacle02.png" []
        , tileRaw "TileSet/floor01.png" []
        ]


layerItems model =
    let
        value =
            round <| animate model.clock model.player
    in
        div [ class "layer" ]
            [ emptyTile
            , tile enemy2 value
            , emptyTile
            , row
            , emptyTile
            , tile player value
            , tile enemy1 value
            , row
            , emptyTile
            , emptyTile
            , emptyTile
            ]


row =
    div [ class "row" ] []


render : Model -> Html Msg
render model =
    div [ class "bigger grid" ]
        [ layerFloor
        , layerItems model
        ]


view : Model -> Html Msg
view model =
    render model

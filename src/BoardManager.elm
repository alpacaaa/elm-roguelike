module BoardManager exposing (..)

import Random
import Time
import Task


type alias Tile =
    {}


type Board
    = Board (List Tile)


createTiles max name =
    List.range 1 max
        |> List.map (\n -> "TileSet/" ++ name ++ "0" ++ (toString n) ++ ".png")


floorTiles =
    createTiles 8 "floor"


wallTiles =
    createTiles 7 "obstacle"


foodTiles =
    createTiles 1 "food"


outerWallTiles =
    createTiles 3 "wall"



-- enemies


columns =
    8


rows =
    8


emptyBoard =
    Board []


createBoard ( outerWalls, floor ) =
    Board []


runGenerator : Random.Generator a -> Float -> a
runGenerator generator now =
    Random.initialSeed (round now)
        |> Random.step generator
        |> Tuple.first


randomTilesGenerator =
    let
        outerWalls =
            ((columns - 2) * 2) + ((rows - 2) * 2) + 4

        floor =
            columns * rows - outerWalls

        wallsList =
            Random.list outerWalls (Random.int 0 <| List.length outerWallTiles)

        floorList =
            Random.list floor (Random.int 0 <| List.length floorTiles)
    in
        Random.pair wallsList floorList


createScene =
    Time.now
        |> Task.map (runGenerator randomTilesGenerator)
        |> Task.map createBoard

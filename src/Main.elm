module Main exposing (..)

import Browser
import Html exposing(div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Dict exposing (Dict)

-- types

type alias Model =
    { currentPlayer : Player
    , gameState : GameState
    , board : Board
    }

type Player
    = Player1
    | Player2

type GameState
    = Won Player
    | Playing

type alias Board =
    { dimensions : ( Int, Int )
    , plays : Plays
    }

type alias Plays =
    Dict Position Player

type alias Position =
    ( Int, Int )

type MoveResult
    = Ok Board Position
    | OutOfBoardError

-- Msg

type Msg
    = Play Int

-- program

main : Program () Model Msg
main =
    Browser.element
    { init = \_ -> init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


-- initial state

init =
    ( initialModel, Cmd.none )

initialModel =
    { currentPlayer = Player1
    , gameState = Playing
    , board = initialBoard
    }

initialBoard =
    { dimensions = ( 7, 7 )
    , plays = Dict.empty
    }

-- update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.gameState of
        Won player ->
            ( model, Cmd.none )

        Playing ->
            case msg of
                Play column ->
                    updatePlaying column model

updatePlaying : Int -> Model -> ( Model, Cmd Msg )
updatePlaying column model =
    case (move column model) of
        OutOfBoardError ->
            ( model, Cmd.none )

        Ok newBoard lastPosition ->
            case winPlay newBoard ( lastPosition, model.currentPlayer ) of
                True ->
                    ( { model
                        | board = newBoard
                        , gameState = Won model.currentPlayer
                      }
                    , Cmd.none
                    )

                False ->
                    ( { model
                        | board = newBoard
                        , currentPlayer = otherPlayer model.currentPlayer
                      }
                    , Cmd.none
                    )

move : Int -> Model -> MoveResult
move column model =
    let
        board = model.board
        player = model.currentPlayer
        position = nextPositionForColumn board column
    in
        case positionInBoard board position of
            True ->
                Ok { board | plays = Dict.insert position player board.plays } position

            False ->
                OutOfBoardError

otherPlayer player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1

-- view

view model =
    div []
    [ headerView model
    , boardView model
    ]

headerView model =
    div [ class "header" ] [
        text (headerText model.gameState model.currentPlayer)
        ]

boardView model =
    div []
    (drawableBoard model.board
            |> List.indexedMap
                (\columnNumber boardColumn ->
                    div [ onClick (Play (columnNumber + 1)) ]
                        (boardColumn
                            |> List.map
                                (\fieldContent ->
                                    boardField fieldContent
                                )
                        )
                )
        )

boardField fieldContent =
    div [style "background-color" (fieldContentToColor fieldContent)]
    [text "-"]

-- view helper functions

headerText : GameState -> Player -> String
headerText gameState currentPlayer =
    case gameState of
        Playing ->
            "Playing: " ++ playerToString currentPlayer

        Won player ->
            (playerToString player) ++ " wins"

playerToString : Player -> String
playerToString player =
    case player of
        Player1 ->
            "Player 1"

        Player2 ->
            "Player 2"

fieldContentToColor : (Maybe Player) -> String
fieldContentToColor fieldContent =
    case fieldContent of
        Nothing -> "white"

        Just Player1 -> "yellow"

        Just Player2 -> "red"


------------------------------------------------
--
--  ,~~.,''"'`'.~~.
-- : {` .- _ -. '} ;
--  `:   O(_)O   ;'
--   ';  ._|_,  ;`
--    '`-.\_/,.'` 
-- 

--- Helper functions we don't care much about

winPlay : Board -> ( Position, Player ) -> Bool
winPlay board ( position, player ) =
    let
        results =
            linesToCheck position
                |> List.map
                    (\line ->
                        line
                            |> List.map (\p -> getPosition board.plays p)
                            |> List.map
                                (\pp ->
                                    if pp == Just player then
                                        1
                                    else
                                        0
                                )
                            |> checkLine
                    )
    in
        results |> List.foldr (||) False

drawableBoard : Board -> List (List (Maybe Player))
drawableBoard board =
    let
        ( columns, rows ) =
            board.dimensions
    in
        List.range 1 columns
            |> List.map
                (\c ->
                    List.range 1 rows
                        |> List.reverse
                        |> List.map
                            (\y ->
                                getPosition board.plays ( c, y )
                            )
                )


checkLine : List Int -> Bool
checkLine line =
    let
        continuousNum =
            line
                |> List.foldl
                    (\y x ->
                        if x == 4 then
                            4
                        else
                            x * y + y
                    )
                    0
    in
        continuousNum == 4

linesToCheck : Position -> List (List Position)
linesToCheck ( x, y ) =
    let
        directions =
            [ ( 0, 1 ) --  |
            , ( 1, 0 ) --  -
            , ( 1, 1 ) --  /
            , ( 1, -1 ) -- \
            ]
    in
        directions
            |> List.map
                (\( a, b ) ->
                    List.range -3 3
                        |> List.map
                            (\mult ->
                                ( a * mult + x, b * mult + y )
                            )
                )

getPosition : Plays -> Position -> Maybe Player
getPosition plays position =
    Dict.get position plays


positionInBoard : Board -> Position -> Bool
positionInBoard board position =
    let
        ( x, y ) =
            board.dimensions

        ( a, b ) =
            position
    in
        List.member a (List.range 1 x) && List.member b (List.range 1 y)


nextPositionForColumn : Board -> Int -> Position
nextPositionForColumn board column =
    let
        nextPosition ( a, b ) =
            case getPosition board.plays ( a, b ) of
                Nothing ->
                    ( a, b )

                _ ->
                    nextPosition ( a, b + 1 )
    in
        nextPosition ( column, 1 )

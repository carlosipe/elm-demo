module GameState exposing(GameState(..), currentPlayer, nextPlayer)

import Player exposing(Player)

type GameState
    = Won Player
    | Playing Player

currentPlayer : GameState -> Player
currentPlayer gameState =
    case gameState of
        Won player ->
            player

        Playing player ->
            player

nextPlayer : GameState -> GameState
nextPlayer gameState =
    case gameState of
        Playing player ->
            Playing(Player.otherPlayer player)
        Won _ ->
            gameState

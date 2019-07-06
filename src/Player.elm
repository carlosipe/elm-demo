module Player exposing(Player(..), toString, fieldContentToColor, otherPlayer, firstPlayer)

type Player
    = Player1
    | Player2

toString : Player -> String
toString player =
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

otherPlayer : Player -> Player
otherPlayer player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1

firstPlayer : Player
firstPlayer =
    Player1

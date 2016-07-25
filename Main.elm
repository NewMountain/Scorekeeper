module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import List exposing (maximum, map)
import Maybe


-- model


type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    , editMode : Bool
    , editPlayerId : Int
    }


type alias Player =
    { playerId : Int
    , name : String
    , points : Int
    }


type alias Play =
    { playId : Int
    , playerId : Int
    , name : String
    , points : Int
    }


initModel : Model
initModel =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    , editMode = False
    , editPlayerId = 0
    }



-- Update


type Msg
    = Edit Player
    | Score Int Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- On new input, update the text in input box
        Input name ->
            { model | name = name }

        --On clicking the save button
        Save ->
            case (model.playerId) of
                -- If the user is being edited, IE already has a playerId
                Just playerId ->
                    model
                        |> updatePlayerNames playerId
                        |> updateNameInPlays playerId
                        |> clearInputBox
                        |> clearPlayerIndex
                        |> deactivateEditMode

                -- If the user is new, IE doesn't yet have a playerId
                Nothing ->
                    model
                        |> addNewPlayer
                        |> clearInputBox
                        |> clearPlayerIndex
                        |> deactivateEditMode

        -- On clicking the cancel button
        Cancel ->
            model
                |> clearInputBox
                |> clearPlayerIndex
                |> deactivateEditMode

        -- When the edit mode is activated by "pencil button" click
        Edit player ->
            model
                |> putNameInInputBox player.name
                |> setPlayerIndex player.playerId
                |> activateEditMode player.playerId

        -- When the two or three point buttons are clicked, update scores
        Score playerId points ->
            model
                |> updatePlayerScore playerId points
                |> appendPlays playerId points

        DeletePlay play ->
            model
                |> removePlay play
                |> updatePlayerScore
                    play.playerId
                    (negate play.points)


removePlay : Play -> Model -> Model
removePlay play model =
    { model | plays = playFilterer model.plays play }


updatePlayerScore : Int -> Int -> Model -> Model
updatePlayerScore playerId points model =
    { model | players = pointUpdater model.players playerId points }


appendPlays : Int -> Int -> Model -> Model
appendPlays playerId points model =
    { model | plays = playMaker model playerId points }


activateEditMode : Int -> Model -> Model
activateEditMode playerId model =
    { model | editMode = True, editPlayerId = playerId }


setPlayerIndex : Int -> Model -> Model
setPlayerIndex playerId model =
    { model | playerId = Just playerId }


putNameInInputBox : String -> Model -> Model
putNameInInputBox name model =
    { model | name = name }


addNewPlayer : Model -> Model
addNewPlayer model =
    { model | players = addPlayer model }


deactivateEditMode : Model -> Model
deactivateEditMode model =
    { model
        | editMode = False
        , editPlayerId = 0
    }


updateNameInPlays : Int -> Model -> Model
updateNameInPlays playerId model =
    { model
        | plays =
            nameUpdater
                model.plays
                playerId
                model.name
    }


clearPlayerIndex : Model -> Model
clearPlayerIndex model =
    { model | playerId = Nothing }


clearInputBox : Model -> Model
clearInputBox model =
    { model | name = "" }


updatePlayerNames : Int -> Model -> Model
updatePlayerNames playerId model =
    { model
        | players =
            nameUpdater
                model.players
                playerId
                model.name
    }


playFilterer : List Play -> Play -> List Play
playFilterer playList play =
    playList
        |> List.filter (\p -> p.playId /= play.playId)


playMaker : Model -> Int -> Int -> List Play
playMaker model playerId points =
    let
        playId =
            model.plays
                |> List.map .playId
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) 1

        playerName =
            model.players
                |> List.map
                    (\player ->
                        case (player.playerId == playerId) of
                            True ->
                                player.name

                            False ->
                                ""
                    )
                |> List.filter (\x -> x /= "")
                |> List.head
                |> Maybe.withDefault ""
    in
        (Play playId playerId playerName points) :: model.plays


nameUpdater :
    List { c | playerId : Int, name : String }
    -> Int
    -> String
    -> List { c | playerId : Int, name : String }
nameUpdater ls playerId name =
    ls
        |> List.map
            (\player ->
                case (player.playerId == playerId) of
                    True ->
                        { player | name = name }

                    False ->
                        player
            )


pointUpdater : List Player -> Int -> Int -> List Player
pointUpdater players playerId points =
    List.map
        (\player ->
            case (player.playerId == playerId) of
                True ->
                    { player | points = player.points + points }

                False ->
                    player
        )
        players


addPlayer : Model -> List Player
addPlayer model =
    let
        -- Map over the list of players, find the max id and increment by one
        id =
            map .playerId model.players
                |> maximum
                |> Maybe.withDefault 0
                |> (+) 1

        name =
            model.name

        points =
            0
    in
        (Player id name points) :: model.players



-- View


view : Model -> Html Msg
view model =
    div
        [ class "scoreboard" ]
        [ h1
            []
            [ text "Score Keeper" ]
        , playerSection model
        , playerForm model
        , playSection model
        ]


playSection : Model -> Html Msg
playSection model =
    div []
        [ playListHeader
        , playList model
        ]


playListHeader : Html Msg
playListHeader =
    header []
        [ div [] [ text "Last 10 Plays" ]
        , div [] [ text "Points" ]
        ]


playList : Model -> Html Msg
playList model =
    model.plays
        |> List.map play
        |> List.take 10
        |> ul []


play : Play -> Html Msg
play play =
    li []
        [ i
            [ class "remove"
            , onClick <| DeletePlay play
            ]
            []
        , div [] [ text play.name ]
        , div [] [ text <| toString play.points ]
        ]


playerSection : Model -> Html Msg
playerSection model =
    div
        []
        [ playerListHeader
        , playerListModel model
        , pointTotal model
        ]


pointTotal : Model -> Html Msg
pointTotal model =
    let
        total =
            List.map .points model.players
                |> List.sum
    in
        footer []
            [ div [] [ text "Total: " ]
            , div [] [ text <| toString total ]
            ]


playerListHeader : Html Msg
playerListHeader =
    header
        []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerListModel : Model -> Html Msg
playerListModel model =
    model.players
        |> List.sortBy .name
        |> List.map (domPlayerMaker model)
        |> ul []


domPlayerMaker : Model -> Player -> Html Msg
domPlayerMaker model player =
    let
        editChecker =
            if
                model.editMode
                    == True
                    && model.editPlayerId
                    == player.playerId
            then
                class "edit"
            else
                class ""
    in
        li []
            [ i
                [ class "edit"
                , onClick (Edit player)
                ]
                []
            , div [ editChecker ]
                [ text player.name ]
            , pointButtonMaker 2 player
            , pointButtonMaker 3 player
            , div []
                [ text <| toString player.points ]
            ]


pointButtonMaker : Int -> Player -> Html Msg
pointButtonMaker i player =
    button
        [ type' "button"
        , onClick <| Score player.playerId i
        ]
        [ text <| toString (i) ++ "pt" ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type' "text"
            , Html.Attributes.placeholder "Add/Edit Player..."
            , onInput Input
            , value model.name
            , if model.editMode == True then
                class "edit"
              else
                class ""
            ]
            []
        , button [ type' "submit" ] [ text "Save" ]
        , button [ type' "button", onClick Cancel ] [ text "Cancel" ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }

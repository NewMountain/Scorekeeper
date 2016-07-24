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
        Input name ->
            { model | name = name }

        Save ->
            case (model.playerId) of
                Just i ->
                    { model
                        | players =
                            nameUpdater
                                model.players
                                i
                                model.name
                        , name = ""
                        , playerId = Nothing
                        , plays =
                            nameUpdater
                                model.plays
                                i
                                model.name
                        , editMode = False
                        , editPlayerId = 0
                    }

                Nothing ->
                    { model
                        | players = addPlayer model
                        , name = ""
                        , playerId = Nothing
                        , editMode = False
                        , editPlayerId = 0
                    }

        Cancel ->
            { model
                | name = ""
                , playerId = Nothing
                , editMode = False
                , editPlayerId = 0
            }

        Edit player ->
            { model
                | name = player.name
                , playerId = Just player.playerId
                , editMode = True
                , editPlayerId = player.playerId
            }

        Score playerId num ->
            -- Find the player in the list and update it
            { model
                | players = pointUpdater model.players playerId num
                , plays = (playMaker model playerId num)
            }

        DeletePlay play ->
            { model
                | plays = playFilterer model.plays play
                , players =
                    pointUpdater
                        model.players
                        play.playerId
                    <|
                        (*) (-1) play.points
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
                |> Debug.log ""
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
    li []
        [ i
            [ class "edit"
            , onClick (Edit player)
            ]
            []
        , div
            [ if
                model.editMode
                    == True
                    && model.editPlayerId
                    == player.playerId
              then
                class "edit"
              else
                class ""
            ]
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

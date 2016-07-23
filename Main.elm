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
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
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
                    }

                Nothing ->
                    { model
                        | players = addPlayer model
                        , name = ""
                        , playerId = Nothing
                    }

        Cancel ->
            { model
                | name = ""
                , playerId = Nothing
            }

        Edit player ->
            { model
                | name = player.name
                , playerId = Just player.id
            }

        Score playerId num ->
            -- Find the player in the list and update it
            { model | players = pointUpdater model.players playerId num }

        _ ->
            model


nameUpdater : List Player -> Int -> String -> List Player
nameUpdater players playerId name =
    List.map
        (\player ->
            case (player.id == playerId) of
                True ->
                    { player | name = name }

                False ->
                    player
        )
        players


pointUpdater : List Player -> Int -> Int -> List Player
pointUpdater players playerId points =
    List.map
        (\player ->
            case (player.id == playerId) of
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
            map .id model.players
                |> maximum
                |> Maybe.withDefault 0
                |> (+) 1

        name =
            model.name

        points =
            0
    in
        model.players ++ [ (Player id name points) ]



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
        , p [] [ text <| toString model ]
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
    ul
        []
        (List.map domPlayerMaker model.players)


domPlayerMaker : Player -> Html Msg
domPlayerMaker player =
    li []
        [ i
            [ class "edit"
            , onClick (Edit player)
            ]
            []
        , div []
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
        , onClick <| Score player.id i
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

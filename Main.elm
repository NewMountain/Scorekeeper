module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import List exposing (maximum, map)


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
    | Score Player Int
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
            { model
                | players = addPlayer model
                , name = ""
            }

        Cancel ->
            { model | name = "" }

        _ ->
            model


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
        , playerForm model
        , p [] [ text <| toString model ]
        ]


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

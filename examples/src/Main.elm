module Main exposing (main)

import Html exposing (..)
import ArrangeableList exposing (..)


-- Model


type alias Model =
    { stuff : ArrangeableList String }



-- Msg


type Msg
    = MoveUp
    | MoveDown



-- Update


init : ( Model, Cmd Msg )
init =
    ( { stuff = fromListAtHead [ "Mabel", "Dipper", "Soos" ] "Gruncle Stan" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- View


listItem : String -> Html Msg
listItem val =
    li [] [ text val ]


view : Model -> Html Msg
view { stuff } =
    let
        pre =
            List.map listItem (getPreList stuff)

        select =
            listItem (getSelected stuff)

        post =
            List.map listItem (getPostList stuff)
    in
        div []
            [ div []
                [ button [] [ text "Move Up" ]
                , button [] [ text "Move Down" ]
                ]
            , ul [] (pre ++ (select :: post))
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

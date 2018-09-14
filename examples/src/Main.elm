module Main exposing (main)

import ArrangeableList exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- Model


type alias Model =
    { stuff : ArrangeableList String }



-- Msg


type Msg
    = MoveUp
    | MoveDown



-- Update


init : flags -> ( Model, Cmd Msg )
init flags =
    ( { stuff = fromListAtHead [ "Mabel", "Dipper", "Soos" ] "Gruncle Stan" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveDown ->
            ( { model | stuff = ArrangeableList.progress model.stuff }, Cmd.none )

        MoveUp ->
            ( { model | stuff = retrogress model.stuff }, Cmd.none )



-- View


listItem : String -> Html Msg
listItem val =
    li [ style "padding" "1rem" ] [ text val ]


view : Model -> Browser.Document Msg
view { stuff } =
    let
        pre =
            List.map listItem (getPreList stuff)

        select =
            li [ style "background-color" "pink", style "padding" "1rem" ] [ text <| getSelected stuff ]

        post =
            List.map listItem (getPostList stuff)
    in
    { title = "Arrangeable List Example"
    , body =
        [ div
            [ style "width" "600px"
            , style "margin" "auto"
            , style "margin-top" "50px"
            , style "font-family" "Raleway, Helvetica, Arial, sans-serif"
            ]
            [ div []
                [ button [ onClick MoveUp ] [ text "Move Up" ]
                , button [ onClick MoveDown ] [ text "Move Down" ]
                ]
            , ul [ style "list-style-type" "none" ] (pre ++ (select :: post))
            ]
        ]
    }


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

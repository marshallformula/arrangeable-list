module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    case msg of
        MoveDown ->
            ( { model | stuff = ArrangeableList.progress model.stuff }, Cmd.none )

        MoveUp ->
            ( { model | stuff = retrogress model.stuff }, Cmd.none )



-- View


listItem : String -> Html Msg
listItem val =
    li [ style [ ( "padding", "1rem" ) ] ] [ text val ]


mainStyle : Attribute Msg
mainStyle =
    style
        [ ( "width", "600px" )
        , ( "margin", "auto" )
        , ( "margin-top", "50px" )
        , ( "font-family", "Raleway, Helvetica, Arial, sans-serif" )
        ]


view : Model -> Html Msg
view { stuff } =
    let
        pre =
            List.map listItem (getPreList stuff)

        select =
            li [ style [ ( "background-color", "pink" ), ( "padding", "1rem" ) ] ] [ text <| getSelected stuff ]

        post =
            List.map listItem (getPostList stuff)
    in
        div [ mainStyle ]
            [ div []
                [ button [ onClick MoveUp ] [ text "Move Up" ]
                , button [ onClick MoveDown ] [ text "Move Down" ]
                ]
            , ul [ style [ ( "list-style-type", "none" ) ] ] (pre ++ (select :: post))
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

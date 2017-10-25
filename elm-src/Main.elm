module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)


view model =
    div []
        [ text "some day you can use this space to describe a wheel"
        , div [ id "outer-circle" ]
            [ text "I'm the outer circle"
            , div [ id "inner-circle" ] [ text "I'm the inner circle" ]
            ]
        ]


type alias Model =
    {
    }


init =
    ( { }, Cmd.none )


type Msg =
    Msg


update msg model =
    ( model, Cmd.none )


main = Html.program
    { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }

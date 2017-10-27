module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Ports
import RemoteData
import Responsibilities
import Return
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


view model =
    div []
        [ Html.text "some day you can use this space to describe a wheel"
        , Responsibilities.view { width = 800, height = 800 } model.selectedWheel
            |> Svg.map WheelMsg
        ]


type alias Model =
    { wheels : RemoteData.RemoteData String (List Responsibilities.Responsibilities)
    , selectedWheel : Responsibilities.Responsibilities
    }


changeResponsibilities : Time.Time -> Model -> Model
changeResponsibilities currentTime model =
    { model | selectedWheel = Responsibilities.rotate currentTime model.selectedWheel }


saveChanges model =
    model.selectedWheel
        |> Responsibilities.encode
        |> Ports.saveWheel


init =
    let
        startingModel =
            Model RemoteData.Loading Responsibilities.simpleWheel
    in
    ( startingModel, Ports.loadWheels {} )


type Msg
    = Nevermind
    | TimeChanged Time.Time
    | WheelMsg Never


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeChanged currentTime ->
            if needToChangeAndSave currentTime model then
                ( model, Cmd.none )
                    |> Return.map (changeResponsibilities currentTime)
                    |> Return.effect_ saveChanges
            else
                ( model, Cmd.none )

        WheelMsg _ ->
            ( model, Cmd.none )

        Nevermind ->
            ( model, Cmd.none )


needToChangeAndSave : Time.Time -> Model -> Bool
needToChangeAndSave currentTime model =
    Responsibilities.isTimeToRotate currentTime model.selectedWheel



-- subscriptions


subscriptions model =
    Ports.timeNow TimeChanged


main =
    Html.program
        { init = init, update = update, view = view, subscriptions = subscriptions }

module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import JobWheel
import Ports
import RemoteData
import Return
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


view : Model -> Html.Html Msg
view model =
    div []
        [ Html.text "some day you can use this space to describe a wheel"
        , viewCurrentJobs { width = 800, height = 800 } model.currentJobs
        ]


viewCurrentJobs : SvgConfig -> TimeDependentState (List JobWheel.ResponsiblePerson) -> Svg.Svg Msg
viewCurrentJobs svgConfig timeDependentState =
    case timeDependentState of
        Known responsiblePeople ->
            let
                personX =
                    "50"

                responsibilityX =
                    150

                peopleCount =
                    List.length <| responsiblePeople

                yPositions =
                    getPositions peopleCount svgConfig.height

                concatTextElements : ( Int, JobWheel.ResponsiblePerson ) -> List (Svg.Svg Msg) -> List (Svg.Svg Msg)
                concatTextElements ( yValue, person ) svgList =
                    let
                        personTextElement =
                            text_
                                [ x personX
                                , y <| toString <| yValue
                                ]
                                [ Svg.text person.name ]

                        maybeJobTextElement =
                            Maybe.map (viewJob responsibilityX yValue) person.job

                        toAppend =
                            case maybeJobTextElement of
                                Just jobTextElement ->
                                    [ personTextElement, jobTextElement ]

                                Nothing ->
                                    [ personTextElement ]
                    in
                    List.append svgList toAppend

                textElements =
                    responsiblePeople
                        |> List.map2 (,) yPositions
                        |> List.foldl concatTextElements []
            in
            svg
                [ svgConfig.width |> toString |> Svg.Attributes.width
                , svgConfig.height |> toString |> Svg.Attributes.height
                ]
                textElements

        Unknown ->
            svg
                [ svgConfig.width |> toString |> Svg.Attributes.width
                , svgConfig.height |> toString |> Svg.Attributes.height
                ]
                [ text_
                    [ x "100"
                    , y "100"
                    ]
                    [ Svg.text "I don't know who is doing what right now." ]
                ]


type alias SvgConfig =
    { width : Int
    , height : Int
    }


viewJob : Int -> Int -> JobWheel.Job -> Svg.Svg Msg
viewJob xVal yVal job =
    text_
        [ x <| toString <| xVal
        , y <| toString <| yVal
        ]
        [ Svg.text job.description ]



-- Model


type alias Model =
    { wheels : RemoteData.RemoteData String (List JobWheel.JobWheel)
    , selectedWheel : JobWheel.JobWheel
    , currentJobs : TimeDependentState (List JobWheel.ResponsiblePerson)
    , timeOfNextChange : TimeDependentState Time.Time
    }


type TimeDependentState a
    = Unknown
    | Known a


determineTimeDependentState : Time.Time -> Model -> Model
determineTimeDependentState time model =
    { model
        | currentJobs = Known (JobWheel.determineJobsAt time model.selectedWheel)
        , timeOfNextChange = Known (JobWheel.timeOfNextChange time model.selectedWheel)
    }


init : ( Model, Cmd Msg )
init =
    let
        startingModel =
            { wheels = RemoteData.Loading
            , selectedWheel = JobWheel.simpleWheel
            , currentJobs = Unknown
            , timeOfNextChange = Unknown
            }
    in
    ( startingModel, Ports.loadWheels () )


type Msg
    = Nevermind
    | TimeReceived Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeReceived currentTime ->
            case model.timeOfNextChange of
                Known time ->
                    if currentTime >= time then
                        ( model, Cmd.none )
                            |> Return.map (determineTimeDependentState currentTime)
                    else
                        ( model, Cmd.none )

                Unknown ->
                    ( model, Cmd.none )
                        |> Return.map (determineTimeDependentState currentTime)

        Nevermind ->
            ( model, Cmd.none )



-- subscriptions


subscriptions model =
    Ports.timeNow TimeReceived



-- main


main =
    Html.program
        { init = init, update = update, view = view, subscriptions = subscriptions }



-- details


getPositions : Int -> Int -> List Int
getPositions howMany availableSpace =
    let
        offset =
            availableSpace // (howMany + 1)
    in
    List.range 1 howMany
        |> List.map (\index -> index * offset)

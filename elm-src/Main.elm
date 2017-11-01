module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
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
        [ viewWheel model
        , viewWheelForm
        ]


viewWheel : Model -> Html.Html Msg
viewWheel model =
    let
        (Entity selectedId selectedWheel) =
            identity model.selectedWheel
    in
    div []
        [ Html.select
            [ onInput SelectedWheelChanged
            , value (selectedId |> toString)
            ]
            (viewOptionsForWheels model)
        , viewCurrentJobs { width = 800, height = 800 } model.currentJobs
        ]


viewOptionsForWheels : Model -> List (Html.Html Msg)
viewOptionsForWheels model =
    let
        optionCurrentlySelected =
            wheelEntityToOptionEl model.selectedWheel

        wheelsToElements : JobWheelList -> List (Html Msg)
        wheelsToElements jobWheelList =
            List.map wheelEntityToOptionEl jobWheelList

        remoteOptionsForOtherWheels : RemoteData.RemoteData String (List (Html Msg))
        remoteOptionsForOtherWheels =
            model.wheels
                |> RemoteData.map wheelsToElements
    in
    case remoteOptionsForOtherWheels of
        RemoteData.Success optionElements ->
            List.append [ optionCurrentlySelected ] optionElements

        _ ->
            [ optionCurrentlySelected ]


wheelEntityToOptionEl : Entity JobWheel.JobWheel -> Html Msg
wheelEntityToOptionEl (Entity id jobWheel) =
    option
        [ value (id |> toString) ] 
        [ Html.text (jobWheel |> JobWheel.describeWheel) ]
            

viewWheelForm : Html.Html Msg
viewWheelForm =
    Html.text "This is the form"


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
    { wheels : RemoteData.RemoteData String JobWheelList
    , selectedWheel : Entity JobWheel.JobWheel
    , currentJobs : TimeDependentState (List JobWheel.ResponsiblePerson)
    , timeOfNextChange : TimeDependentState Time.Time
    }


changeSelectedWheel : Entity JobWheel.JobWheel -> Model -> Model
changeSelectedWheel newlySelected model =
    { model
        | selectedWheel = newlySelected
        , currentJobs = Unknown
        , timeOfNextChange = Unknown
    }


type alias JobWheelList =
    List (Entity JobWheel.JobWheel)


findWheel : Int -> RemoteData.RemoteData String JobWheelList -> Result String (Entity JobWheel.JobWheel)
findWheel id remoteWheelList =
    case remoteWheelList of
        RemoteData.Success jobWheelList ->
            let
                idsMatch : Entity (JobWheel.JobWheel) -> Bool
                idsMatch (Entity someId _) =
                    someId == id
            in
            jobWheelList |> firstInList idsMatch

        _ ->
            Err "don't have wheel list"
    


type Entity a =
    Entity Int a


justTheValue : Entity a -> a
justTheValue (Entity id value) =
    value


type TimeDependentState a
    = Unknown
    | Known a


determineTimeDependentState : Time.Time -> Model -> Model
determineTimeDependentState time model =
    let
        selectedWheel =
            justTheValue model.selectedWheel
    in
    { model
        | currentJobs = Known (JobWheel.determineJobsAt time selectedWheel)
        , timeOfNextChange = Known (JobWheel.timeOfNextChange time selectedWheel)
    }


init : ( Model, Cmd Msg )
init =
    let
        startingModel =
            { wheels = RemoteData.Loading
            , selectedWheel = Entity 0 JobWheel.simpleWheel
            , currentJobs = Unknown
            , timeOfNextChange = Unknown
            }
    in
    ( startingModel, Ports.loadWheels () )


type Msg
    = Nevermind
    | TimeReceived Time.Time
    | SelectedWheelChanged String


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

        SelectedWheelChanged newValue ->
            let
                findWheelResult =
                    newValue
                        |> String.toInt
                        |> Result.andThen (\id -> findWheel id model.wheels)
            in
            case findWheelResult of
                Ok wheel -> 
                    ( model, Cmd.none )
                        |> Return.map (changeSelectedWheel wheel)

                Err _->
                    ( model, Cmd.none )

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


firstInList : (a -> Bool) -> List a -> Result String a
firstInList checkMatch someList =
    case List.head someList of
        Just item ->
            if checkMatch item then
                Ok item

            else
                firstInList checkMatch (List.drop 1 someList)

        Nothing ->
            Err ""

module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import JobWheel
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import RemoteData
import Return
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time
import WheelForm


view : Model -> Html.Html Msg
view model =
    div []
        [ viewWheel model
        , hr [] []
        , viewError model.error
        , (WheelForm.view model.wheelForm) |> Html.map WheelFormMsg
        , button
            [ onClick MakeCreateCmd ]
            [ Html.text "Looks good. Make it so." ]
        ]


viewWheel : Model -> Html.Html Msg
viewWheel model =
    let
        (Entity selectedId selectedWheel) =
            identity model.selectedWheel

        jobsSvg =
            case model.displayMode of
                RealTime ->
                    viewRealTime { width = 800, height = 800 } (model.selectedWheel |> justTheValue)

                Static ->
                    viewCurrentJobs { width = 800, height = 800 } model.currentJobs
    in
    div []
        [ Html.select
            [ onInput SelectedWheelChanged
            , value (selectedId |> toString)
            ]
            (viewOptionsForWheels model)
        , Html.label [ for "show-realtime" ] [ Html.text "display in real time" ]
        , Html.input
            [ Html.Attributes.id "show-realtime"
            , Html.Attributes.type_ "checkbox"
            , onClick ToggleDisplayMode
            ] [ ]
        , jobsSvg
        ]


viewRealTime : SvgConfig -> JobWheel.JobWheel -> Svg Msg
viewRealTime svgConfig model =
    Svg.text <| "angle of rotation is 14 degrees. Always."


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


viewError : Maybe String -> Html Msg
viewError error =
    let
        textNode =
            case error of
                Just someString ->
                    Html.text someString

                Nothing ->
                    Html.text ""
    in
    p [ Html.Attributes.class "error-message" ] [ textNode ]


-- Model


type alias Model =
    { wheels : RemoteData.RemoteData String JobWheelList
    , selectedWheel : Entity JobWheel.JobWheel
    , currentJobs : TimeDependentState (List JobWheel.ResponsiblePerson)
    , timeOfNextChange : TimeDependentState Time.Time
    , wheelForm : WheelForm.WheelForm
    , error : Maybe String
    , displayMode : DisplayMode
    }


addDistinctWheels : JobWheelList -> Model -> Model
addDistinctWheels wheels model =
    case model.wheels of
        RemoteData.Success existingWheels ->
            let
                existingIds =
                    List.map justTheId existingWheels

                newAndDistinct =
                    List.filter (\(Entity id jobWheel) -> not <| (List.member id existingIds)) wheels

                updatedWheels =
                    newAndDistinct 
                        |> List.append existingWheels
                        |> RemoteData.Success
            in
            { model | wheels = updatedWheels }

        _ ->
            { model | wheels = RemoteData.Success wheels }


setError : String -> Model -> Model
setError theError model =
    { model | error = Just theError }


type DisplayMode
    = RealTime
    | Static


type ParticipantCountValue
    = EmptyString
    | MoreThanOne Int


toParticipantCountValue : String -> Result String ParticipantCountValue
toParticipantCountValue someString =
    if someString == "" then
        Ok EmptyString

    else
        someString
            |> String.toInt
            |> Result.andThen toMoreThanOne


toMoreThanOne : Int -> Result String ParticipantCountValue
toMoreThanOne someInt =
    if someInt > 1 then
        Ok <| MoreThanOne <| someInt

    else
        Err "gotta have more than one"


countValueToString : ParticipantCountValue -> String
countValueToString countValue =
    case countValue of
        MoreThanOne value ->
            value |> toString

        EmptyString ->
            Debug.log "empty string is interesting" ""


type alias Participant =
    { name : String
    , job : String
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


getNextId : List (Entity a) -> Int
getNextId entities =
    case List.head <| sortDescending <| List.map justTheId entities of
        Just anId ->
            anId + 1

        Nothing ->
            1


justTheValue : Entity a -> a
justTheValue (Entity id value) =
    value


justTheId : Entity a -> Int
justTheId (Entity id value) =
    id


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
        ( startingWheelForm, wheelFormCmd ) =
            WheelForm.init
                |> Return.mapCmd WheelFormMsg

        startingModel =
            { wheels = RemoteData.NotAsked
            , selectedWheel = Entity 0 JobWheel.simpleWheel
            , currentJobs = Unknown
            , timeOfNextChange = Unknown
            , wheelForm = startingWheelForm
            , error = Nothing
            , displayMode = Static
            }

        startingCmd =
            wheelFormCmd
    in
    ( startingModel, wheelFormCmd )


type Msg
    = Nevermind
    | TimeReceived Time.Time
    | SelectedWheelChanged String
    | WheelFormMsg WheelForm.Msg
    | MakeCreateCmd
    | MakeSaveCmd JobWheel.JobWheel
    | WheelsReceived JobWheelList
    | ErrorCreatingJobWheel String
    | ToggleDisplayMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDisplayMode ->
            let
                newDisplayMode =
                    case model.displayMode of
                        RealTime ->
                            Static

                        Static ->
                            RealTime
            in
            ( { model | displayMode = newDisplayMode }, Cmd.none )

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

        WheelFormMsg wheelFormMsg ->
            WheelForm.update wheelFormMsg model.wheelForm
                |> Return.map (\updatedWheelForm -> { model | wheelForm = updatedWheelForm })
                |> Return.mapCmd WheelFormMsg

        MakeCreateCmd ->
            let
                wheelResultToMsg wheelResult =
                    case wheelResult of
                        Ok jobWheel ->
                            MakeSaveCmd jobWheel

                        Err error ->
                            Debug.log "create wheel error: " error
                                |> always Nevermind
                
                makeJobWheel time =
                    case JobWheel.makeJobWheel time (WheelForm.getFormData model.wheelForm) of
                        Ok jobWheel ->
                            Task.succeed jobWheel

                        Err error ->
                            Task.fail error

                creationTask =
                    Time.now
                        |> Task.andThen makeJobWheel 
            in
            ( model, Cmd.none )
                |> Return.command (Task.attempt wheelResultToMsg creationTask)

        MakeSaveCmd jobWheel ->
            let
                encodedJobWheel =
                    JobWheel.encode jobWheel
            in
            ( model, Cmd.none )
                |> Return.command (Ports.saveWheelCmd encodedJobWheel)

        WheelsReceived wheels ->
            ( model, Cmd.none )
                |> Return.map (addDistinctWheels wheels)

        ErrorCreatingJobWheel error ->
            ( model, Cmd.none )
                |> Return.map (setError error)

        Nevermind ->
            ( model, Cmd.none )



-- subscriptions


subscriptions model =
    Sub.batch
        [ Time.every Time.second TimeReceived
        , Ports.wheels wheelsJsonToMsg
        ]


wheelsJsonToMsg : Encode.Value -> Msg
wheelsJsonToMsg json =
    case Decode.decodeValue wheelsDecoder json of
        Ok wheels ->
            WheelsReceived wheels

        Err error ->
            Debug.log "decoding error: " error
                |> always Nevermind


wheelsDecoder : Decode.Decoder JobWheelList
wheelsDecoder =
    Decode.list <|
        Decode.map2
            Entity
            (Decode.field "id" Decode.int)
            JobWheel.jobWheelDecoder


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


toZeroOrGreater : Int -> Result () Int
toZeroOrGreater someInt =
    if someInt >= 0 then
        Ok someInt

    else
        Err ()


sortDescending : List comparable -> List comparable
sortDescending someList =
    someList
        |> List.sortWith flippedComparison


flippedComparison a b =
    case compare a b of
    LT -> GT
    EQ -> EQ
    GT -> LT

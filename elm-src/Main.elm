module Main exposing (main)

import Angle
import FloatOps
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import JobWheel
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import Return
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time
import WheelForm
import WheelView


view : Model -> Html.Html Msg
view model =
    div []
        [ viewWheel model
        , hr [] []
        , WheelForm.view model.wheelForm |> Html.map WheelFormMsg
        , button
            [ onClick MakeCreateCmd ]
            [ Html.text "Looks good. Make it so." ]
        , viewError model.error
        ]


type alias SvgConfig =
    { class : String
    , fontSize : String
    , width : Int
    , height : Int
    }


svgConfig : SvgConfig
svgConfig =
    { class = "wheel-view"
    , fontSize = "18"
    , width = 800
    , height = 800
    }


viewWheel : Model -> Html.Html Msg
viewWheel model =
    let
        (Entity selectedId selectedWheel) =
            identity model.selectedWheel

        jobsSvg =
            viewJobs model.displayMode model.wheelOrientation
    in
    div []
        [ jobsSvg
        , Html.select
            [ Html.Attributes.class "row"
            , on
                "change"
                (Decode.map SelectedWheelChanged targetValue)
            , value (selectedId |> toString)
            ]
            (viewOptionsForWheels model)
        , Html.input
            [ Html.Attributes.id "show-realtime"
            , Html.Attributes.type_ "checkbox"
            , onClick ToggleDisplayMode
            ]
            []
        , Html.label [ for "show-realtime" ] [ Html.text "display in real time" ]
        ]


viewJobs : DisplayMode -> TimeDependentState WheelOrientation -> Html Msg
viewJobs displayMode wheelOrientation =
    let
        viewFunction =
            case displayMode of
                RealTime ->
                    WheelView.viewAsClock

                Static ->
                    WheelView.viewWheel
    in
    case wheelOrientation of
        Known wheelOrientation ->
            let
                wheelView =
                    viewFunction
                        svgConfig
                        ( wheelOrientation.personList, Angle.fromDegrees wheelOrientation.angleInDegrees )
            in
            wheelView |> Html.map (always WheelViewMsg)

        Unknown ->
            viewLoading svgConfig


viewLoading : SvgConfig -> Html Msg
viewLoading svgConfig =
    svg
        [ Svg.Attributes.width (svgConfig.width |> toString)
        , Svg.Attributes.height (svgConfig.height |> toString)
        ]
        [ text_
            [ fontSize "30"
            , x ((svgConfig.width // 2) |> toString)
            , y ((svgConfig.height // 2) |> toString)
            , textAnchor "middle"
            ]
            [ Svg.text "Loading" ]
        ]


viewOptionsForWheels : Model -> List (Html.Html Msg)
viewOptionsForWheels model =
    let
        wheelsToElements : JobWheelList -> List (Html Msg)
        wheelsToElements jobWheelList =
            List.map wheelEntityToOptionEl jobWheelList
    in
    wheelsToElements model.wheels


wheelEntityToOptionEl : Entity JobWheel.JobWheel -> Html Msg
wheelEntityToOptionEl (Entity id jobWheel) =
    option
        [ value (id |> toString) ]
        [ Html.text (jobWheel |> JobWheel.describeWheel) ]


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
    span [ Html.Attributes.class "error-message" ] [ textNode ]



-- Model


type alias Model =
    { wheels : JobWheelList
    , selectedWheel : Entity JobWheel.JobWheel
    , wheelOrientation : TimeDependentState WheelOrientation
    , currentJobs : TimeDependentState (List JobWheel.ResponsiblePerson)
    , timeOfNextChange : TimeDependentState Time.Time
    , wheelForm : WheelForm.WheelForm
    , error : Maybe String
    , displayMode : DisplayMode
    }


type alias WheelOrientation =
    { personList : List Person
    , angleInDegrees : Float
    , angleInRadians : Float
    }


type alias Person =
    { name : String
    , job : Maybe String
    }


changeWheelOrientation : WheelOrientation -> Model -> Model
changeWheelOrientation orientation model =
    { model | wheelOrientation = Known orientation }


addDistinctWheels : JobWheelList -> Model -> Model
addDistinctWheels wheels model =
    let
        existingIds =
            List.map justTheId model.wheels

        newAndDistinct =
            List.filter (\(Entity id jobWheel) -> not <| List.member id existingIds) wheels

        updatedWheels =
            newAndDistinct
                |> List.append model.wheels
    in
    { model | wheels = updatedWheels }


setError : String -> Model -> Model
setError theError model =
    { model | error = Just theError }


type DisplayMode
    = RealTime
    | Static


changeSelectedWheel : Entity JobWheel.JobWheel -> Model -> Model
changeSelectedWheel newlySelected model =
    { model
        | selectedWheel = newlySelected
        , currentJobs = Unknown
        , timeOfNextChange = Unknown
    }


type alias JobWheelList =
    List (Entity JobWheel.JobWheel)


findWheel : Int -> JobWheelList -> Result String (Entity JobWheel.JobWheel)
findWheel id wheelList =
    let
        idsMatch : Entity JobWheel.JobWheel -> Bool
        idsMatch (Entity someId _) =
            someId == id
    in
    wheelList |> firstInList idsMatch


type Entity a
    = Entity Int a


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

        simpleWheelEntity =
            Entity 0 JobWheel.simpleWheel

        startingModel =
            { wheels = [ simpleWheelEntity ]
            , selectedWheel = simpleWheelEntity
            , wheelOrientation = Unknown
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
    | WheelViewMsg


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
            if timeToChange currentTime model.timeOfNextChange model.displayMode then
                let
                    ( personList, angle ) =
                        getOrientation model.displayMode currentTime (model.selectedWheel |> justTheValue)

                    reducedTurns =
                        Angle.inRadians angle
                            / (2 * pi)
                            |> FloatOps.justTheDecimalPart

                    reducedDegrees =
                        reducedTurns * 360

                    wheelOrientation =
                        { personList = personList
                        , angleInDegrees = reducedDegrees
                        , angleInRadians = degrees reducedDegrees
                        }
                in
                ( model, Cmd.none )
                    |> Return.map (determineTimeDependentState currentTime)
                    |> Return.map (changeWheelOrientation wheelOrientation)
            else
                ( model, Cmd.none )

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

                Err _ ->
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
                                |> ErrorCreatingJobWheel

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
                |> Return.map (\theModel -> { theModel | error = Nothing })
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

        WheelViewMsg ->
            ( model, Cmd.none )

        Nevermind ->
            ( model, Cmd.none )


timeToChange : Time.Time -> TimeDependentState Time.Time -> DisplayMode -> Bool
timeToChange currentTime appointedTime displayMode =
    case displayMode of
        Static ->
            case appointedTime of
                Known time ->
                    if currentTime >= time then
                        True
                    else
                        False

                Unknown ->
                    True

        RealTime ->
            True



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


getOrientation : DisplayMode -> Time.Time -> JobWheel.JobWheel -> ( List Person, Angle.AngleOfRotation )
getOrientation displayMode time jobWheel =
    let
        orientationFunction =
            case displayMode of
                Static ->
                    JobWheel.getStaticOrientation

                RealTime ->
                    JobWheel.getRealTimeOrientation
    in
    orientationFunction time jobWheel

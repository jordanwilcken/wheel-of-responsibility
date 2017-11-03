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
        , hr [] []
        , viewWheelForm model
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
            

viewWheelForm : Model -> Html.Html Msg
viewWheelForm model =
    div []
        [ p [ ] [ Html.text "Make a new Job Wheel" ]
        , button [] [ Html.text "like the one above" ]
        , br [ ] [ ]
        , label [ for "people-count" ] [ Html.text "How many participants in your job wheel? (min is 2; max is 20)" ]
        , input
            [ Html.Attributes.id "people-count"
            , value <| (model.wheelForm.participantCountValue |> countValueToString)
            , onInput ParticipantCountInputChanged
            ] []   
        , viewParticipantInputs model.wheelForm
        , Html.text "preview:"
        , Html.text "this is the preview"
        , Html.text "how often should participants rotate jobs"
        , button [ ] [ Html.text "Looks good. Make it so." ]
        ]


viewParticipantInputs : WheelForm -> Html Msg
viewParticipantInputs wheelForm =
    div []
        [ Html.text "look at us go!"
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
    { wheels : RemoteData.RemoteData String JobWheelList
    , selectedWheel : Entity JobWheel.JobWheel
    , currentJobs : TimeDependentState (List JobWheel.ResponsiblePerson)
    , timeOfNextChange : TimeDependentState Time.Time
    , wheelForm : WheelForm
    }


changeParticipantCount : ParticipantCountValue -> Model -> Model
changeParticipantCount count model =
    let
        ( participantInt, constrainedValue ) =
            case count of
                EmptyString ->
                    ( 0, count )

                MoreThanOne proposed ->
                    if proposed > maxParticipants then
                        ( maxParticipants, maxParticipants |> MoreThanOne )

                    else
                        ( proposed, count )

        difference =
            (Debug.log "should be constrained: " participantInt) - (countParticipants model.wheelForm)

        updatedParticipants =
            if difference > 0 then
                List.repeat difference { name = "", job = "" }
                    |> List.append model.wheelForm.participants
                
            else
                model.wheelForm.participants
                    |> List.take participantInt

        originalWheelForm =
            model.wheelForm

        updatedWheelForm =
            { originalWheelForm
                | participants = updatedParticipants
                , participantCountValue = constrainedValue
            }
            
    in
    { model | wheelForm = updatedWheelForm }


maxParticipants : Int
maxParticipants =
    20


type alias WheelForm =
    { participants : List Participant
    , participantCountValue : ParticipantCountValue
    , maxParticipants : Int
    }


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


countParticipants : WheelForm -> Int
countParticipants wheelForm =
    List.length wheelForm.participants


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
        startingWheelForm =
            { participants = [ ]
            , maxParticipants = maxParticipants
            , participantCountValue = EmptyString
            }

        startingModel =
            { wheels = RemoteData.Loading
            , selectedWheel = Entity 0 JobWheel.simpleWheel
            , currentJobs = Unknown
            , timeOfNextChange = Unknown
            , wheelForm = startingWheelForm
            }
    in
    ( startingModel, Ports.loadWheels () )


type Msg
    = Nevermind
    | TimeReceived Time.Time
    | SelectedWheelChanged String
    | ParticipantCountInputChanged String


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

        ParticipantCountInputChanged newValue ->
            case newValue |> toParticipantCountValue of
                Ok validValue ->
                    ( model, Cmd.none )
                        |> Return.map (changeParticipantCount validValue)

                Err _ ->
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


toZeroOrGreater : Int -> Result () Int
toZeroOrGreater someInt =
    if someInt >= 0 then
        Ok someInt

    else
        Err ()

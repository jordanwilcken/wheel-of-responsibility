module WheelForm exposing (FormData, Msg, WheelForm, encodeFormData, getFormData, init, update, view)

import Angle
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, targetValue)
import Json.Decode as Decode
import Json.Encode as Encode
import Return
import Time
import WheelView


view : WheelForm -> Html Msg
view (WheelForm wheelForm) =
    div []
        [ h1 [] [ Html.text "Make a new Job Wheel" ]
        , div
            [ class "row" ]
            [ label
                [ class "input-label"
                , for "wheel-description"
                ]
                [ Html.text "What will you call this one?" ]
            , input
                [ id "wheel-description"
                , onInput SetDescription
                , value wheelForm.description
                ]
                []
            ]
        , div [ class "row" ]
            [ label
                [ class "input-label"
                , for "people-count"
                ]
                [ Html.text "How many participants? (min 2, max 20)" ]
            , input
                [ id "people-count"
                , attribute "min" "2"
                , attribute "max" (maxParticipants |> toString)
                , type_ "number"
                , onInput participantCountStringToMsg
                ]
                []
            ]
        , viewParticipantInputs wheelForm.participants
        , p [] [ Html.text "preview:" ]
        , viewPreview
            { class = "wheel-preview"
            , fontSize = "18"
            , height = 600
            , width = 600
            }
            wheelForm.participants
        , viewTimeUnitSelect
        ]


viewTimeUnitSelect : Html Msg
viewTimeUnitSelect =
    div
        [ class "row" ]
        [ span
            [ class "input-label" ]
            [ text "How often should participants rotate jobs?" ]
        , span []
            [ label
                [ class "input-label"
                , for "rotation-interval"
                ]
                [ text "every" ]
            , input
                [ id "rotation-interval"
                , attribute "max" "100"
                , attribute "min" (minInterval |> toString)
                , type_ "number"
                , onInput intervalStringToMsg
                ]
                []
            , select
                [ on
                    "change"
                    (Decode.map unitStringToMsg targetValue)
                ]
                [ option
                    [ value "seconds" ]
                    [ text "seconds" ]
                , option
                    [ value "minutes" ]
                    [ text "minutes" ]
                , option
                    [ value "hours" ]
                    [ text "hours" ]
                , option
                    [ value "days" ]
                    [ text "days" ]
                , option
                    [ value "weeks" ]
                    [ text "weeks" ]
                ]
            ]
        ]


viewPreview : WheelView.SvgConfig -> List Participant -> Html Msg
viewPreview svgConfig participants =
    let
        thePreview =
            WheelView.viewWheel
                svgConfig
                ( participants |> List.map toPerson
                , Angle.fromDegrees 0
                )
    in
    thePreview |> Html.map PreviewMsg


viewParticipantInputs : List Participant -> Html Msg
viewParticipantInputs participants =
    div [] (participants |> List.indexedMap viewNameAndJobInputs)


viewNameAndJobInputs : Int -> Participant -> Html Msg
viewNameAndJobInputs index participant =
    let
        nameId =
            "participant-name-" ++ (index + 1 |> toString)

        jobId =
            "job-description-" ++ (index + 1 |> toString)
    in
    div [ class "row" ]
        [ label
            [ class "input-label"
            , for nameId
            ]
            [ text "name" ]
        , input
            [ id nameId
            , class "wheel-input left-column-input"
            , onInput (NameChanged index)
            , value participant.name
            ]
            []
        , label
            [ class "input-label"
            , for jobId
            ]
            [ text "job" ]
        , input
            [ for jobId
            , class "wheel-input"
            , onInput (JobChanged index)
            , value participant.job
            ]
            []
        ]


type WheelForm
    = WheelForm
        { description : String
        , participants : List Participant
        , rotationInterval : RotationInterval
        }


type alias RotationInterval =
    { amount : Int
    , timeUnits : TimeUnits
    }


toTime : RotationInterval -> Time.Time
toTime rotationInterval =
    let
        day =
            Time.hour * 24

        week =
            day * 7

        theUnit =
            case rotationInterval.timeUnits of
                Weeks ->
                    week

                Days ->
                    day

                Hours ->
                    Time.hour

                Minutes ->
                    Time.minute

                Seconds ->
                    Time.second
    in
    (rotationInterval.amount |> toFloat) * theUnit


changeIntervalAmount : Int -> WheelForm -> WheelForm
changeIntervalAmount newAmount (WheelForm wheelForm) =
    let
        currentInterval =
            wheelForm.rotationInterval

        updated =
            { currentInterval | amount = newAmount }
    in
    { wheelForm | rotationInterval = updated }
        |> WheelForm


changeIntervalUnits : TimeUnits -> WheelForm -> WheelForm
changeIntervalUnits newUnits (WheelForm wheelForm) =
    let
        currentInterval =
            wheelForm.rotationInterval

        updated =
            { currentInterval | timeUnits = newUnits }
    in
    { wheelForm | rotationInterval = updated }
        |> WheelForm


changeNameAt : Int -> String -> WheelForm -> WheelForm
changeNameAt index newValue (WheelForm wheelForm) =
    let
        newList =
            changeAt index (\person -> { person | name = newValue }) wheelForm.participants
    in
    { wheelForm | participants = newList }
        |> WheelForm


changeJobAt : Int -> String -> WheelForm -> WheelForm
changeJobAt index newValue (WheelForm wheelForm) =
    let
        newList =
            changeAt index (\person -> { person | job = newValue }) wheelForm.participants
    in
    { wheelForm | participants = newList }
        |> WheelForm


changeAt : Int -> (a -> a) -> List a -> List a
changeAt index makeChange originalList =
    let
        toTake =
            index

        toSwap =
            originalList
                |> List.drop toTake
                |> List.head
                |> Maybe.map makeChange
    in
    case toSwap of
        Just newItem ->
            let
                preceding =
                    List.take toTake originalList

                following =
                    List.drop (toTake + 1) originalList
            in
            List.concat
                [ preceding
                , [ newItem ]
                , following
                ]

        Nothing ->
            originalList


maxParticipants : Int
maxParticipants =
    20


newParticipant : Participant
newParticipant =
    { name = "Mystery Date"
    , job = "be charming"
    }


sizeParticipants : Int -> WheelForm -> WheelForm
sizeParticipants size (WheelForm wheelForm) =
    let
        count =
            if size > maxParticipants then
                maxParticipants
            else
                size

        difference =
            count - (wheelForm.participants |> List.length)

        updatedParticipants =
            if difference > 0 then
                List.repeat difference newParticipant
                    |> List.append wheelForm.participants
            else
                wheelForm.participants
                    |> List.take count
    in
    WheelForm
        { wheelForm | participants = updatedParticipants }


type alias Participant =
    { name : String
    , job : String
    }


type alias Person =
    { name : String
    , job : Maybe String
    }


toPerson : Participant -> Person
toPerson participant =
    let
        job =
            if String.length participant.job > 0 then
                Just participant.job
            else
                Nothing
    in
    Person
        participant.name
        job


init : ( WheelForm, Cmd Msg )
init =
    let
        participants =
            [ { name = "Leonardo", job = "invent stuff" }
            , { name = "Donatello", job = "sculpt David" }
            , { name = "Michaelangelo", job = "paint the chapel" }
            , { name = "Raphael", job = "wash the Turle Van" }
            ]
    in
    ( WheelForm
        { description = "new job wheel"
        , participants = participants
        , rotationInterval = { amount = minInterval, timeUnits = Seconds }
        }
    , Cmd.none
    )


type Msg
    = ParticipantsWanted Int
    | PreviewMsg Never
    | SetDescription String
    | NameChanged Int String
    | JobChanged Int String
    | IntervalAmountChanged Int
    | IntervalUnitsChanged TimeUnits
    | Nevermind


participantCountStringToMsg : String -> Msg
participantCountStringToMsg someString =
    let
        resultToMsg : Result String Int -> Msg
        resultToMsg someResult =
            case someResult of
                Ok desiredNumber ->
                    ParticipantsWanted desiredNumber

                Err _ ->
                    Nevermind
    in
    someString
        |> String.toInt
        |> Result.andThen (toMinimum 2)
        |> resultToMsg


intervalStringToMsg : String -> Msg
intervalStringToMsg someString =
    let
        resultToMsg : Result String Int -> Msg
        resultToMsg theResult =
            case theResult of
                Ok theInterval ->
                    IntervalAmountChanged theInterval

                Err _ ->
                    Nevermind
    in
    someString
        |> String.toInt
        |> Result.andThen (toMinimum minInterval)
        |> resultToMsg


unitStringToMsg : String -> Msg
unitStringToMsg someString =
    let
        maybeToMsg : Maybe TimeUnits -> Msg
        maybeToMsg maybeTimeUnits =
            case maybeTimeUnits of
                Just timeUnits ->
                    IntervalUnitsChanged timeUnits

                Nothing ->
                    Nevermind
    in
    [ ( "weeks", Weeks )
    , ( "days", Days )
    , ( "hours", Hours )
    , ( "minutes", Minutes )
    , ( "seconds", Seconds )
    ]
        |> Dict.fromList
        |> Dict.get someString
        |> maybeToMsg


minInterval : Int
minInterval =
    5


type TimeUnits
    = Weeks
    | Days
    | Hours
    | Minutes
    | Seconds


update : Msg -> WheelForm -> ( WheelForm, Cmd Msg )
update msg wheelForm =
    case msg of
        ParticipantsWanted numberWanted ->
            ( wheelForm, Cmd.none )
                |> Return.map (sizeParticipants numberWanted)

        NameChanged index newValue ->
            ( wheelForm, Cmd.none )
                |> Return.map (changeNameAt index newValue)

        JobChanged index newValue ->
            ( wheelForm, Cmd.none )
                |> Return.map (changeJobAt index newValue)

        IntervalAmountChanged newInterval ->
            ( wheelForm, Cmd.none )
                |> Return.map (changeIntervalAmount newInterval)

        IntervalUnitsChanged newUnits ->
            ( wheelForm, Cmd.none )
                |> Return.map (changeIntervalUnits newUnits)

        SetDescription newDescription ->
            case wheelForm of
                WheelForm theForm ->
                    ( WheelForm { theForm | description = newDescription }, Cmd.none )

        PreviewMsg _ ->
            ( wheelForm, Cmd.none )

        Nevermind ->
            ( wheelForm, Cmd.none )


type alias FormData =
    { description : String
    , participants : List Participant
    , rotationInterval : Time.Time
    }


getFormData : WheelForm -> FormData
getFormData (WheelForm wheelForm) =
    FormData
        wheelForm.description
        wheelForm.participants
        (wheelForm.rotationInterval |> toTime)


encodeFormData : FormData -> Encode.Value
encodeFormData formData =
    Encode.object
        [ ( "description", Encode.string formData.description )
        , ( "participants", encodeParticipants formData.participants )
        , ( "rotationInterval", Encode.float formData.rotationInterval )
        ]


encodeParticipants : List Participant -> Encode.Value
encodeParticipants participants =
    participants
        |> List.map
            (\item ->
                Encode.object
                    [ ( "name", Encode.string item.name )
                    , ( "job", Encode.string item.job )
                    ]
            )
        |> Encode.list



-- details


toMinimum : Int -> Int -> Result String Int
toMinimum theMinimum someInt =
    if someInt >= theMinimum then
        Ok someInt
    else
        Err "too small"

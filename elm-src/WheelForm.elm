module WheelForm exposing (Msg, WheelForm, init, countParticipants, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Return
import StaticJobView


view : WheelForm -> Html Msg
view (WheelForm wheelForm) =
    div []
        [ p [ ] [ Html.text "Make a new Job Wheel" ]
        , button [] [ Html.text "like the one above" ]
        , br [ ] [ ]
        , label [ for "people-count" ] [ Html.text "How many participants in your job wheel? (min is 2; max is 20)" ]
        , input
            [ id "people-count"
            , step "1"
            , attribute "min" "2"
            , attribute "max" (maxParticipants |> toString)
            , type_ "number"
            , onInput ParticipantCountInputChanged
            ] []   
        , viewParticipantInputs wheelForm.participants
        , p [ ] [ Html.text "preview:" ]
        , viewPreview { height = 600, width = 600 } wheelForm.participants
        , Html.text "how often should participants rotate jobs"
        , button [ ] [ Html.text "Looks good. Make it so." ]
        ]


viewPreview : StaticJobView.SvgConfig -> List Participant -> Html Msg
viewPreview svgConfig participants =
    let
        simpleView =
            StaticJobView.simpleView
                svgConfig
                (participants |> List.map toStaticJobViewPerson)
    in
    simpleView |> Html.map PreviewMsg


viewParticipantInputs : (List Participant) -> Html Msg
viewParticipantInputs participants =
    let
        viewSingle : Int -> Participant -> Html Msg
        viewSingle index participant =
            let
                nameId =
                    "participant-name-" ++ (index + 1 |> toString)

                jobId =
                    "job-description-" ++ (index + 1 |> toString)
            in
            div []
                [ label
                    [ for nameId ]
                    [ text "name" ]
                , input
                    [ id nameId
                    , class "wheel-input"
                    , onInput (NameChanged index)
                    , value participant.name
                    ]
                    [ ]
                , label
                    [ for jobId ]
                    [ text "job" ]
                , input
                    [ for jobId
                    , class "wheel-input"
                    , onInput (JobChanged index)
                    , value participant.job
                    ]
                    [ ]
                ]
    in
    div []
        (participants |> List.indexedMap viewSingle)


type WheelForm =
    WheelForm
    { participants : List Participant
    , participantCount : ValidParticipantCount
    }


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


countParticipants : WheelForm -> Int
countParticipants (WheelForm wheelForm) =
    List.length wheelForm.participants


maxParticipants : Int
maxParticipants =
    20


changeParticipantCount : ValidParticipantCount -> WheelForm -> WheelForm
changeParticipantCount validCount (WheelForm wheelForm) =
    let
        count =
            case validCount of
                ValidParticipantCount value ->
                    value

        difference =
            (Debug.log "should be constrained: " count) - (wheelForm.participants |> List.length)

        updatedParticipants =
            if difference > 0 then
                List.repeat difference { name = "", job = "" }
                    |> List.append wheelForm.participants
                
            else
                wheelForm.participants
                    |> List.take count
    in
    WheelForm
        { wheelForm
            | participants = updatedParticipants
            , participantCount = validCount
        }


type alias Participant =
    { name : String
    , job : String
    }


toStaticJobViewPerson : Participant -> StaticJobView.Person
toStaticJobViewPerson participant =
    let
        job =
            if String.length participant.job > 0 then
                Just participant.job

            else
                Nothing
    in
    StaticJobView.Person
        participant.name
        job

            


type ValidParticipantCount
    = ValidParticipantCount Int


getValidParticipantCount : String -> Result String ValidParticipantCount
getValidParticipantCount someString =
    let
        toValidCount : Int -> Result String ValidParticipantCount
        toValidCount someInt =
            if someInt > 1 && someInt <= maxParticipants then
                someInt |> ValidParticipantCount |> Ok

            else
                Err "out of bounds"
    in
    someString
        |> String.toInt
        |> Result.andThen toValidCount


init : ( WheelForm, Cmd Msg )
init =
    let
        participants =
            [ { name = "somebody", job = "do stuff" }
            , { name = "somebody else", job = "do the other stuff" }
            ]
    in
    ( WheelForm
        { participants = participants
        , participantCount = participants |> List.length |> ValidParticipantCount
        }
    , Cmd.none )


type Msg
    = ParticipantCountInputChanged String
    | PreviewMsg Never
    | NameChanged Int String
    | JobChanged Int String


update : Msg -> WheelForm -> ( WheelForm, Cmd Msg )
update msg wheelForm =
    case msg of
        ParticipantCountInputChanged newValue ->
            case newValue |> getValidParticipantCount of
                Ok validValue ->
                    ( wheelForm, Cmd.none )
                        |> Return.map (changeParticipantCount validValue)

                Err _ ->
                    ( wheelForm, Cmd.none )

        NameChanged index newValue ->
            ( wheelForm, Cmd.none )
                |> Return.map (changeNameAt index newValue)

        JobChanged index newValue ->
            ( wheelForm, Cmd.none )
                |> Return.map (changeJobAt index newValue)

        PreviewMsg _ ->
            ( wheelForm, Cmd.none )

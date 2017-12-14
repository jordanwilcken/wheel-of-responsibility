module JobWheel
    exposing
        ( JobWheel
        , ResponsiblePerson
        , describeWheel
        , determineJobsAt
        , encode
        , getRealTimeOrientation
        , getStaticOrientation
        , jobWheelDecoder
        , makeJobWheel
        , simpleWheel
        , timeOfNextChange
        )

import Angle
import FloatOps
import Json.Decode as Decode
import Json.Encode as Encode
import Regex
import Time


getRealTimeOrientation : Time.Time -> JobWheel -> ( List { name : String, job : Maybe String }, Angle.AngleOfRotation )
getRealTimeOrientation time jobWheel =
    getOrientation (angleOfRotation time jobWheel) jobWheel


getStaticOrientation : Time.Time -> JobWheel -> ( List { name : String, job : Maybe String }, Angle.AngleOfRotation )
getStaticOrientation time jobWheel =
    let
        angle : Angle.AngleOfRotation
        angle =
            changesThisTurn time jobWheel
                |> floor
                |> toFloat
                |> (*) (rotationPerChange jobWheel |> Angle.inRadians)
                |> Angle.fromRadians
    in
    getOrientation angle jobWheel


getOrientation : Angle.AngleOfRotation -> JobWheel -> ( List { name : String, job : Maybe String }, Angle.AngleOfRotation )
getOrientation angle (JobWheel jobWheel) =
    ( jobWheel.origin |> toNamesAndJobs, angle )


simpleWheel : JobWheel
simpleWheel =
    JobWheel
        { description = "Example Job Wheel"
        , timeCreated = 0
        , period = 25 * Time.second
        , origin = simplePeople
        , rotationDirection = Clockwise
        }


makeJobWheel : Time.Time -> FormData -> Result String JobWheel
makeJobWheel theTime formData =
    let
        constructor validDescription responsiblePeople =
            JobWheel
                { description = validDescription
                , timeCreated = theTime
                , period = calculatePeriod formData.rotationInterval responsiblePeople
                , origin = responsiblePeople
                , rotationDirection = Clockwise
                }
    in
    Result.map2
        constructor
        (getValidDescription formData.description)
        (getResponsiblePeople formData.participants)


type alias FormData =
    { description : String
    , participants : List { name : String, job : String }
    , rotationInterval : Time.Time
    }


simplePeople : ResponsiblePeople
simplePeople =
    { first = ResponsiblePerson "This" (Just (Job "the participants"))
    , middle =
        [ ResponsiblePerson "is" (Just (Job "change jobs"))
        , ResponsiblePerson "a" (Just (Job "every"))
        , ResponsiblePerson "job" (Just (Job "5"))
        ]
    , last = ResponsiblePerson "wheel" (Just (Job "seconds"))
    }


angleOfRotation : Time.Time -> JobWheel -> Angle.AngleOfRotation
angleOfRotation time (JobWheel record) =
    let
        periods =
            (time - record.timeCreated) / record.period

        wheelTurns =
            case record.rotationDirection of
                Clockwise ->
                    negate periods

                CounterClockwise ->
                    periods
    in
    turns wheelTurns
        |> Angle.fromRadians


describeWheel : JobWheel -> String
describeWheel (JobWheel record) =
    record.description


timeOfNextChange : Time.Time -> JobWheel -> Time.Time
timeOfNextChange time jobWheel =
    time + changeInterval jobWheel - timeSinceLastChange time jobWheel


timeSinceLastChange : Time.Time -> JobWheel -> Time.Time
timeSinceLastChange time jobWheel =
    changesThisTurn time jobWheel
        |> FloatOps.justTheDecimalPart
        |> (*) (changeInterval jobWheel)


changesThisTurn : Time.Time -> JobWheel -> Float
changesThisTurn time jobWheel =
    let
        currentTurnDecimal =
            turnDecimal time jobWheel
    in
    (changesPerTurn jobWheel |> toFloat) * currentTurnDecimal


rotationPerChange : JobWheel -> Angle.AngleOfRotation
rotationPerChange (JobWheel jobWheel) =
    let
        angle =
            (2 * pi) / (changesPerTurn (JobWheel jobWheel) |> toFloat)
    in
    case jobWheel.rotationDirection of
        Clockwise ->
            negate angle |> Angle.fromRadians

        CounterClockwise ->
            angle |> Angle.fromRadians


changesPerTurn : JobWheel -> Int
changesPerTurn (JobWheel record) =
    countPeople record.origin


changeInterval : JobWheel -> Time.Time
changeInterval (JobWheel record) =
    record.period / (countPeople record.origin |> toFloat)


determineJobsAt : Time.Time -> JobWheel -> List ResponsiblePerson
determineJobsAt time (JobWheel record) =
    let
        jobSwitchesPerTurn =
            countPeople record.origin

        theTurnDecimal =
            turnDecimal time (JobWheel record)

        jobSwitches =
            (jobSwitchesPerTurn |> toFloat) * theTurnDecimal

        switchesRounded =
            if jobSwitches >= 0 then
                floor jobSwitches
            else
                ceiling jobSwitches

        personList =
            rotateJobsNTimes switchesRounded record.origin
    in
    personList


rotateJobsNTimes : Int -> ResponsiblePeople -> List ResponsiblePerson
rotateJobsNTimes n responsiblePeople =
    let
        startingList =
            listPeople responsiblePeople

        peopleCount =
            List.length startingList

        simplifiedN =
            rem n peopleCount

        positiveN =
            if simplifiedN >= 0 then
                simplifiedN
            else
                peopleCount + simplifiedN

        rotatedJobs =
            startingList
                |> List.map .job
                |> shiftList positiveN
    in
    List.map2
        ResponsiblePerson
        (List.map .name startingList)
        rotatedJobs


turnDecimal : Time.Time -> JobWheel -> Float
turnDecimal time (JobWheel record) =
    let
        turns =
            (time - record.timeCreated) / record.period
    in
    if turns >= 0 then
        getPositiveDecimal turns
    else
        getNegativeDecimal turns


getPositiveDecimal : Float -> Float
getPositiveDecimal theFloat =
    theFloat - (theFloat |> floor |> toFloat)


getNegativeDecimal : Float -> Float
getNegativeDecimal theFloat =
    theFloat - (theFloat |> ceiling |> toFloat)


listPeople : ResponsiblePeople -> List ResponsiblePerson
listPeople people =
    people.first :: List.append people.middle [ people.last ]


toNamesAndJobs : ResponsiblePeople -> List { name : String, job : Maybe String }
toNamesAndJobs responsiblePeople =
    let
        mapToRecords =
            List.map (\person -> { name = person.name, job = Maybe.map .description person.job })
    in
    responsiblePeople
        |> listPeople
        |> mapToRecords


type Direction
    = Clockwise
    | CounterClockwise


directionDecoder : Decode.Decoder Direction
directionDecoder =
    let
        stringToDirection someString =
            if someString == "counterClockwise" then
                CounterClockwise
            else
                Clockwise
    in
    Decode.string
        |> Decode.map stringToDirection


type JobWheel
    = JobWheel
        { description : String
        , timeCreated : Time.Time
        , period : Time.Time
        , origin : ResponsiblePeople
        , rotationDirection : Direction
        }


jobWheelDecoder : Decode.Decoder JobWheel
jobWheelDecoder =
    Decode.map5
        (\description timeCreated period origin rotationDirection ->
            JobWheel
                { description = description
                , timeCreated = timeCreated
                , period = period
                , origin = origin
                , rotationDirection = rotationDirection
                }
        )
        (Decode.field "description" Decode.string)
        (Decode.field "timeCreated" Decode.float)
        (Decode.field "period" Decode.float)
        (Decode.field "origin" originDecoder)
        (Decode.field "rotationDirection" directionDecoder)


encode : JobWheel -> Encode.Value
encode (JobWheel jobWheel) =
    let
        encodeJob : Maybe Job -> Encode.Value
        encodeJob job =
            case job of
                Just aJob ->
                    Encode.string aJob.description

                Nothing ->
                    Encode.string ""

        encodePerson : ResponsiblePerson -> Encode.Value
        encodePerson person =
            Encode.object
                [ ( "name", Encode.string person.name )
                , ( "job", encodeJob person.job )
                ]

        encodeResponsiblePeople : ResponsiblePeople -> Encode.Value
        encodeResponsiblePeople responsiblePeople =
            responsiblePeople
                |> listPeople
                |> List.map encodePerson
                |> Encode.list

        encodeDirection : Direction -> Encode.Value
        encodeDirection direction =
            case direction of
                Clockwise ->
                    Encode.string "clockwise"

                CounterClockwise ->
                    Encode.string "counterClockwise"
    in
    Encode.object
        [ ( "description", Encode.string jobWheel.description )
        , ( "timeCreated", Encode.float jobWheel.timeCreated )
        , ( "period", Encode.float jobWheel.period )
        , ( "origin", encodeResponsiblePeople jobWheel.origin )
        , ( "rotationDirection", encodeDirection jobWheel.rotationDirection )
        ]


type alias ResponsiblePeople =
    { first : ResponsiblePerson
    , middle : List ResponsiblePerson
    , last : ResponsiblePerson
    }


originDecoder : Decode.Decoder ResponsiblePeople
originDecoder =
    let
        makeResponsiblePeopleDecoder : List ResponsiblePerson -> Decode.Decoder ResponsiblePeople
        makeResponsiblePeopleDecoder personList =
            case toResponsiblePeople personList of
                Ok responsiblePeople ->
                    Decode.succeed responsiblePeople

                Err error ->
                    Decode.fail error
    in
    Decode.list responsiblePersonDecoder
        |> Decode.andThen makeResponsiblePeopleDecoder


responsiblePersonDecoder : Decode.Decoder ResponsiblePerson
responsiblePersonDecoder =
    Decode.map2
        ResponsiblePerson
        (Decode.field "name" Decode.string)
        (Decode.field "job" jobDecoder)


jobDecoder : Decode.Decoder (Maybe Job)
jobDecoder =
    let
        stringToJob : String -> Maybe Job
        stringToJob someString =
            if String.length someString > 0 then
                Just <| Job someString
            else
                Nothing
    in
    Decode.string
        |> Decode.map stringToJob


toResponsiblePeople : List ResponsiblePerson -> Result String ResponsiblePeople
toResponsiblePeople personList =
    let
        first =
            case List.head personList of
                Just responsiblePerson ->
                    Ok responsiblePerson

                Nothing ->
                    Err "Your job wheel needs more participants"

        toTake =
            List.length personList - 2

        middle =
            personList
                |> List.drop 1
                |> List.take toTake
                |> Ok

        last =
            case getLast personList of
                Just responsiblePerson ->
                    Ok responsiblePerson

                Nothing ->
                    Err "Your job wheel needs more participants"
    in
    Result.map3
        ResponsiblePeople
        first
        middle
        last


getResponsiblePeople : List { name : String, job : String } -> Result String ResponsiblePeople
getResponsiblePeople participants =
    let
        toResponsibleList : List (Result String ResponsiblePerson) -> Result String (List ResponsiblePerson)
        toResponsibleList results =
            case List.head results of
                Nothing ->
                    Ok []

                Just resultItem ->
                    case resultItem of
                        Ok responsiblePerson ->
                            toResponsibleList (List.drop 1 results) |> Result.map ((::) responsiblePerson)

                        Err error ->
                            Err error
    in
    participants
        |> List.map toResponsiblePerson
        |> toResponsibleList
        |> Result.andThen toResponsiblePeople


toResponsiblePerson : { name : String, job : String } -> Result String ResponsiblePerson
toResponsiblePerson record =
    let
        name =
            if allWhitespace record.name then
                Err "You can't have names consisting only of white space."
            else
                Ok record.name

        job =
            if allWhitespace record.job then
                Nothing
            else
                Just (Job record.job)
    in
    name |> Result.map (\theName -> ResponsiblePerson theName job)


allWhitespace : String -> Bool
allWhitespace someString =
    Regex.contains (Regex.regex "^\\s*$") someString


calculatePeriod : Time.Time -> ResponsiblePeople -> Time.Time
calculatePeriod rotationInterval responsiblePeople =
    rotationInterval * (countPeople responsiblePeople |> toFloat)


countPeople : ResponsiblePeople -> Int
countPeople people =
    1 + List.length people.middle + 1


type alias Person =
    { name : String
    }


type alias ResponsiblePerson =
    { name : String
    , job : Maybe Job
    }


toPerson : ResponsiblePerson -> Person
toPerson responsiblePerson =
    Person responsiblePerson.name


type alias Job =
    { description : String
    }



-- details


shiftList : Int -> List a -> List a
shiftList howMuch someList =
    if howMuch < 0 then
        shiftTowardsHead howMuch someList
    else if howMuch > 0 then
        shiftTowardsTail howMuch someList
    else
        someList


shiftTowardsTail : Int -> List a -> List a
shiftTowardsTail howMuch someList =
    if howMuch < 0 then
        shiftTowardsHead (negate howMuch) someList
    else if howMuch == 0 then
        someList
    else
        shiftTowardsTail (howMuch - 1) (lastToFirst someList)


shiftTowardsHead : Int -> List a -> List a
shiftTowardsHead howMuch someList =
    if howMuch < 0 then
        shiftTowardsTail (negate howMuch) someList
    else if howMuch == 0 then
        someList
    else
        shiftTowardsHead (howMuch - 1) (firstToLast someList)


getLast : List a -> Maybe a
getLast someList =
    someList
        |> List.reverse
        |> List.head


dropLast : List a -> List a
dropLast someList =
    let
        numberToTake =
            List.length someList - 1
    in
    List.take numberToTake someList


lastToFirst : List a -> List a
lastToFirst someList =
    case getLast someList of
        Just item ->
            item
                :: someList
                |> dropLast

        Nothing ->
            someList


firstToLast : List a -> List a
firstToLast someList =
    case List.head someList of
        Just item ->
            someList
                |> (\list -> List.append list [ item ])
                |> List.drop 1

        Nothing ->
            someList



-- details


getValidDescription : String -> Result String String
getValidDescription proposedDescription =
    if allWhitespace proposedDescription then
        Err "You can't have a description that is all white space."
    else
        Ok proposedDescription

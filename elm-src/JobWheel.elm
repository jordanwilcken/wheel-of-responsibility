module JobWheel
    exposing
        ( Job
        , JobWheel
        , ResponsiblePerson
        , determineJobsAt
        , simpleWheel
        , timeOfNextChange
        )

import Time


simpleWheel : JobWheel
simpleWheel =
    JobWheel
        { timeCreated = 0
        , period = 4 * Time.second
        , origin = simplePeople
        , rotationDirection = Clockwise
        }


simplePeople : ResponsiblePeople
simplePeople =
    { first = ResponsiblePerson 1 "Jim" (Just (Job 1 "float like a butterfly"))
    , middle = []
    , last = ResponsiblePerson 2 "Bob" (Just (Job 2 "sting like a bee"))
    }


angleOfRotation : Time.Time -> JobWheel -> Float
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


timeOfNextChange : Time.Time -> JobWheel -> Time.Time
timeOfNextChange time jobWheel =
    time + changeInterval jobWheel - timeSinceLastChange time jobWheel


timeSinceLastChange : Time.Time -> JobWheel -> Time.Time
timeSinceLastChange time jobWheel =
    changesThisTurn time jobWheel
        |> justTheDecimalPart
        |> (*) (changeInterval jobWheel)


changesThisTurn : Time.Time -> JobWheel -> Float
changesThisTurn time jobWheel =
    let
        currentTurnDecimal =
            turnDecimal time jobWheel
    in
    (changesPerTurn jobWheel |> toFloat) * currentTurnDecimal


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
    List.map3
        ResponsiblePerson
        (List.map .id startingList)
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


type Direction
    = Clockwise
    | CounterClockwise


type JobWheel
    = JobWheel
        { timeCreated : Time.Time
        , period : Time.Time
        , origin : ResponsiblePeople
        , rotationDirection : Direction
        }


type alias ResponsiblePeople =
    { first : ResponsiblePerson
    , middle : List ResponsiblePerson
    , last : ResponsiblePerson
    }


countPeople : ResponsiblePeople -> Int
countPeople people =
    1 + List.length people.middle + 1


type alias Person =
    { id : Int
    , name : String
    }


type alias ResponsiblePerson =
    { id : Int
    , name : String
    , job : Maybe Job
    }


toPerson : ResponsiblePerson -> Person
toPerson responsiblePerson =
    Person responsiblePerson.id responsiblePerson.name


type alias Job =
    { id : Int
    , description : String
    }



-- details


justTheDecimalPart : Float -> Float
justTheDecimalPart someFloat =
    let
        negateIfNecessary : Float -> Float
        negateIfNecessary theDecimalPart =
            if someFloat < 0 then
                negate theDecimalPart
            else
                theDecimalPart
    in
    someFloat
        |> abs
        |> floor
        |> toFloat
        |> (-) someFloat
        |> negateIfNecessary


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

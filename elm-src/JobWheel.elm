module JobWheel exposing
    ( JobWheel, determineJobsAt, simpleWheel, timeOfNextChange
    , Job
    , ResponsiblePerson
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
        { first = (ResponsiblePerson (Person 1 "Jim") (Just (Job 1 "float like a butterfly")))
        , middle = [ ]
        , last = (ResponsiblePerson (Person 2 "Bob") (Just (Job 2 "sting like a bee")))
        }


--rotateJobsOnce : Direction -> JobWheel -> JobWheel
--rotateJobsOnce direction (JobWheel record) =
--  let
--    firstToLast : List a -> List a
--    firstToLast someList =
--        case List.head someList of
--            Just item ->
--                someList
--                  |> (\list -> List.append list [ item ])
--                  |> List.drop 1
--            
--            Nothing ->
--                someList
--
--    getLast : List a -> Maybe a
--    getLast someList =
--        someList
--          |> List.reverse
--          |> List.head
--
--    dropLast : List a -> List a
--    dropLast someList =
--      let
--          numberToTake = (List.length someList) - 1
--      in
--          List.take numberToTake someList
--
--    lastToFirst someList =
--      case getLast someList of
--        Just item ->
--          item :: someList
--            |> dropLast
--        
--        Nothing ->
--            someList
--
--    shuffleJobs : List Job -> List Job
--    shuffleJobs =
--        case direction of
--          Clockwise ->
--            firstToLast
--          
--          CounterClockwise ->
--            lastToFirst
--
--    jobs =
--      record.participants
--        |> listPeople
--        |> List.map .job
--        |> shuffleJobs
--        
--  in
--  -- { record
--  --   | participants =
--  --       List.map2
--  --         ResponsiblePerson
--  --         (justThePeople record.participants)
--  --         (justTheJobs record.participants)
--  -- }
--  JobWheel record

-- in radians
-- clockwise rotations are negative
-- Elm provides a function "turns" that converts turns to radians
calculateAngleOfRotation : Time.Time -> JobWheel -> Float
calculateAngleOfRotation time (JobWheel record) =
    let
        periods =
            ( time - record.timeCreated ) / record.period
        
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
    time + (getChangeInterval jobWheel) - (timeSinceLastChange time jobWheel)
        

timeSinceLastChange : Time.Time -> JobWheel -> Time.Time
timeSinceLastChange time jobWheel =
    changesThisTurn time jobWheel
        |> justTheDecimal
        |> (*) (getChangeInterval jobWheel)


changesThisTurn : Time.Time -> JobWheel -> Float
changesThisTurn time jobWheel =
    let
        changesPerTurn =
            case jobWheel of
                JobWheel record ->
                    countPeople record.origin
    in
    (toFloat changesPerTurn) * (turnDecimal time jobWheel)


getChangeInterval : JobWheel -> Time.Time
getChangeInterval (JobWheel record) =
    record.period / ((countPeople record.origin) |> toFloat)


justTheDecimal : Float -> Float
justTheDecimal someFloat =
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

        justPeople =
            startingList |> List.map .person

        rotatedJobs =
            startingList
                |> List.map .job
                |> shiftList positiveN
    in
    List.map2
        ResponsiblePerson
        justPeople
        rotatedJobs


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
      numberToTake = (List.length someList) - 1
  in
      List.take numberToTake someList


lastToFirst : List a -> List a
lastToFirst someList =
  case getLast someList of
    Just item ->
      item :: someList
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

type JobWheel =
  JobWheel
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
    1 + (List.length people.middle) + 1

type alias Person =
  { id : Int
  , name : String
  }

type alias ResponsiblePerson =
  { person : Person
  , job : Maybe Job
  }

type alias Job =
  { id : Int
  , description : String
  }

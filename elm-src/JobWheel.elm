module JobWheel exposing (doStuff)

import Time


doStuff () =
  "done"


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
    , participants : ResponsiblePeople
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

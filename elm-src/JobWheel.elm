module JobWheel exposing (doStuff)

import Time


doStuff () =
  "done"


getJobs : Time.Time -> JobWheel -> List Person

rotateJobsOnce : Direction -> JobWheel -> JobWheel
rotateJobsOnce direction (JobWheel record) =
  let
    firstToLast : List a -> List a
    firstToLast someList =
        case List.head someList of
            Just item ->
                someList
                  |> (\list -> List.append list [ item ])
                  |> List.drop 1
            
            Nothing ->
                someList

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

    lastToFirst someList =
      case getLast someList of
        Just item ->
          item :: someList
            |> dropLast
        
        Nothing ->
            someList

    shuffleJobs : List Job -> List Job
    shuffleJobs =
        case direction of
          Clockwise ->
            firstToLast
          
          CounterClockwise ->
            lastToFirst

    jobs =
      record.participants
        |> listPeople
        |> List.map .job
        |> shuffleJobs
        
  in
  -- { record
  --   | participants =
  --       List.map2
  --         ResponsiblePerson
  --         (justThePeople record.participants)
  --         (justTheJobs record.participants)
  -- }
  JobWheel record

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
                    - periods

                CounterClockwise ->
                    periods
    in
    turns wheelTurns
        

determineJobsAt : Time.Time -> JobWheel -> List ResponsiblePerson
determineJobsAt time (JobWheel record) =
    let
        jobSwitchesPerTurn =
            countPeople record.origin

        jobSwitches =
             jobSwitchesPerTurn * (decimalPartOfTurns time (JobWheel record))

        personList =
            rotateJobsNTimes jobSwitches record.origin
    in
    personList


listPeople : People -> List Person
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

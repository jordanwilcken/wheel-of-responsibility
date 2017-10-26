module Responsibilities
    exposing
        ( Responsibilities
        , encode
        , isTimeToRotate
        , rotate
        , simpleWheel
        , view
        )

import Json.Encode as Encode
import Svg exposing (..)
import Time


view : Responsibilities -> Svg.Svg Never
view (Responsibilities record) =
    svg []
        [ text "Hello again, world!"
        ]


type Responsibilities
    = Responsibilities
        { rotationInterval : Time.Time
        , timeCreated : Time.Time
        , timeLastRotated : Maybe Time.Time
        , responsibilityList : List Responsibility
        , responsiblePeople : People
        }


isTimeToRotate : Time.Time -> Responsibilities -> Bool
isTimeToRotate currentTime (Responsibilities record) =
    case record.timeLastRotated of
        Just timeLastRotated ->
            currentTime >= (timeLastRotated + record.rotationInterval)

        Nothing ->
            currentTime >= (record.timeCreated + record.rotationInterval)


simpleWheel : Responsibilities
simpleWheel =
    let
        jim =
            Person 1 "Jim"

        bob =
            Person 2 "Bob"

        thePeople =
            People jim [] bob
    in
    Responsibilities
        { rotationInterval = 5 * Time.second
        , timeCreated = 0
        , timeLastRotated = Maybe.Nothing
        , responsibilityList = simpleResponsibilityList thePeople
        , responsiblePeople = thePeople
        }


simpleResponsibilityList : People -> List Responsibility
simpleResponsibilityList people =
    [ Responsibility 1 "float like a butterfly" people.first people.last
    , Responsibility 2 "sting like a bee" people.last people.first
    ]


rotate : Time.Time -> Responsibilities -> Responsibilities
rotate currentTime (Responsibilities record) =
    record.responsibilityList
        |> List.map (\responsibility -> changeAssignee (\person -> nextAfter person record.responsiblePeople) responsibility)
        |> (\newList ->
                { record
                    | responsibilityList = newList
                    , timeLastRotated = Just currentTime
                }
           )
        |> Responsibilities


encode responsibilities =
    Encode.object
        [ ( "todo", Encode.string "encode for reals" )
        ]


type alias Responsibility =
    { id : Int
    , description : String
    , assignee : Person
    , nextAssignee : Person
    }


changeAssignee : (Person -> Person) -> Responsibility -> Responsibility
changeAssignee nextAfter responsibility =
    responsibility
        |> (\theResponsibility -> { theResponsibility | assignee = theResponsibility.nextAssignee })
        |> (\theResponsibility -> { theResponsibility | nextAssignee = nextAfter theResponsibility.assignee })


type alias Person =
    { id : Int
    , name : String
    }


type alias People =
    { first : Person
    , middle : List Person
    , last : Person
    }


nextAfter : Person -> People -> Person
nextAfter person people =
    if person == people.first then
        case List.head people.middle of
            Just thisOne ->
                thisOne

            Nothing ->
                people.last
    else if person == people.last then
        people.first
    else
        case nextInList person people.middle of
            Just thisOne ->
                thisOne

            Nothing ->
                people.last


nextInList : Person -> List Person -> Maybe Person
nextInList person list =
    case List.head list of
        Nothing ->
            Nothing

        Just somePerson ->
            if somePerson == person then
                list
                    |> List.drop 1
                    |> List.head
            else
                nextInList person (List.drop 1 list)

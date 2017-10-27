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
import Svg.Attributes exposing (..)
import Time


type alias SvgConfig =
    { width : Int
    , height : Int
    }


view : SvgConfig -> Responsibilities -> Svg.Svg Never
view svgConfig (Responsibilities record) =
    let
        personX =
            50

        responsibilityX =
            150

        peopleCount =
            List.length <| peopleToList record.responsiblePeople

        responsibilityCount =
            List.length record.responsibilityList           

        yPositions =
            getPositions peopleCount svgConfig.height

        personPositions =
            List.map3
                (\xVal yVal person ->
                    { x = xVal
                    , y = yVal
                    , person = person
                    })
                (List.repeat peopleCount personX)
                yPositions
                (peopleToList record.responsiblePeople)
            
        personTextElements =
            List.map
                (\item ->
                    text_
                        [ item.x |> toString |> x
                        , item.y |> toString |> y
                        ]
                        [ text item.person.name ])
                personPositions

        toResponsibilityPositions : Responsibility -> Result () { x : Int, y : Int, responsibility : Responsibility }
        toResponsibilityPositions responsibility =
            let
                matches =
                    List.filter (\item -> item.person == responsibility.assignee) personPositions
            in
            case List.head matches of
                Just personPosition ->
                    Ok { x = responsibilityX, y = personPosition.y, responsibility = responsibility }

                Nothing ->
                    Err ()

        justTheSuccesses : List (Result e a) -> List a
        justTheSuccesses results =
            case List.head results of
                Just result ->
                    case result of
                        Ok data ->
                            data :: justTheSuccesses (List.drop 1 results)
                        
                        Err _ ->
                            justTheSuccesses (List.drop 1 results)

                Nothing ->
                    [ ]
                    
        
        responsibilityTextElements =
            record.responsibilityList
                |> List.map toResponsibilityPositions
                |> justTheSuccesses
                |> List.map (\positioned ->
                    text_
                        [ positioned.x |> toString |> x
                        , positioned.y |> toString |> y
                        ]
                        [ text positioned.responsibility.description ])
    in
    svg
        [ svgConfig.width |> toString |> width
        , svgConfig.height |> toString |> height
        ]
        (List.append personTextElements responsibilityTextElements)


getPositions : Int -> Int -> List Int
getPositions howMany availableSpace =
    let
        offset =
            availableSpace // (howMany + 1)
    in
    List.range 1 howMany
        |> List.map (\index -> index * offset)


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


peopleToList : People -> List Person
peopleToList people =
    people.first :: List.append people.middle [ people.last ]
    


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

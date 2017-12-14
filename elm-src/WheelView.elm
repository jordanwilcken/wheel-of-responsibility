module WheelView exposing (SvgConfig, viewAsClock, viewWheel)

import Angle exposing (..)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import SvgSpace


viewAsClock : SvgConfig -> ( List Person, Angle.AngleOfRotation ) -> Html.Html Never
viewAsClock svgConfig ( people, angleOfRotation ) =
    viewCircles drawHands svgConfig ( people, angleOfRotation )


viewWheel : SvgConfig -> ( List Person, Angle.AngleOfRotation ) -> Html.Html Never
viewWheel svgConfig ( people, angleOfRotation ) =
    viewCircles drawWedges svgConfig ( people, angleOfRotation )


viewCircles : MarkCircle -> SvgConfig -> ( List Person, Angle.AngleOfRotation ) -> Html.Html Never
viewCircles markInnerCircle svgConfig ( people, angleOfRotation ) =
    let
        center =
            { x = svgConfig.width // 2
            , y = svgConfig.height // 2
            }

        jobDescription person =
            case person.job of
                Just description ->
                    description

                Nothing ->
                    ""

        jobs =
            List.map jobDescription people

        names =
            List.map .name people

        outerCircle =
            { angleOfRotation = Angle.fromDegrees 0
            , centerX = center.x
            , centerY = center.y
            , radius = (45 * svgConfig.width) // 100
            , sectionCount = List.length people
            }

        innerCircle =
            { angleOfRotation = angleOfRotation
            , centerX = center.x
            , centerY = center.y
            , radius = (outerCircle.radius * 80) // 100
            , sectionCount = List.length people
            }
    in
    svg
        [ class svgConfig.class
        , fontSize svgConfig.fontSize
        , svgConfig.width |> toString |> Svg.Attributes.width
        , svgConfig.height |> toString |> Svg.Attributes.height
        ]
    <|
        List.concat
            [ [ circle
                    [ cx <| (center.x |> toString)
                    , cy <| (center.y |> toString)
                    , fill "tan"
                    , r (outerCircle.radius |> toString)
                    ]
                    []
              ]
            , drawWedges names outerCircle
            , [ circle
                    [ cx <| (center.x |> toString)
                    , cy <| (center.y |> toString)
                    , fill "lightblue"
                    , r (innerCircle.radius |> toString)
                    , stroke "black"
                    , strokeWidth "4"
                    ]
                    []
              ]
            , markInnerCircle jobs innerCircle
            ]


drawHands : List String -> Circle -> List (Svg Never)
drawHands strings circle =
    makeInnerCircleElements
        (stringToClockHand circle)
        strings
        circle


drawWedges : List String -> Circle -> List (Svg Never)
drawWedges strings circle =
    makeInnerCircleElements
        (stringToWedge circle)
        strings
        circle


makeInnerCircleElements : (Float -> String -> List (Svg Never)) -> List String -> Circle -> List (Svg Never)
makeInnerCircleElements stringToElements strings circle =
    let
        transformAngles =
            strings |> List.indexedMap (\index _ -> calcLineTransformAngle circle index)

        nestedElements =
            List.map2
                stringToElements
                transformAngles
                strings
    in
    List.concat nestedElements


stringToClockHand : Circle -> Float -> String -> List (Svg Never)
stringToClockHand circle lineTransformAngle someString =
    let
        vars =
            makeClockHandVars circle lineTransformAngle
    in
    [ text_
        [ x (vars.centerX |> toString)
        , y (vars.textY |> toString)
        , textAnchor "middle"
        , makeRotateTransform vars.stringTransformAngle { cx = vars.centerX, cy = vars.centerY }
        ]
        [ text someString ]
    , polygon
        [ points vars.trianglePointsString
        , fill "black"
        , makeRotateTransform vars.stringTransformAngle { cx = vars.centerX, cy = vars.centerY }
        ]
        []
    , line
        [ x1 (vars.centerX |> toString)
        , y1 (vars.centerY |> toString)
        , x2 (vars.centerX + ((vars.radiusFloat * cos vars.lineAngle) |> floor) |> toString)
        , y2 (vars.centerY - ((vars.radiusFloat * sin vars.lineAngle) |> floor) |> toString)
        , stroke "black"
        , strokeWidth "4"
        , makeRotateTransform lineTransformAngle { cx = vars.centerX, cy = vars.centerY }
        ]
        []
    ]


stringToWedge : Circle -> Float -> String -> List (Svg Never)
stringToWedge circle lineTransformAngle someString =
    let
        vars =
            makeWedgeVars circle lineTransformAngle
    in
    [ text_
        [ x (vars.centerX |> toString)
        , y (vars.textY |> toString)
        , textAnchor "middle"
        , makeRotateTransform lineTransformAngle { cx = circle.centerX, cy = circle.centerY }
        ]
        [ text someString ]
    , line
        [ x1 (circle.centerX |> toString)
        , y1 (circle.centerY |> toString)
        , x2 (circle.centerX + ((vars.radiusFloat * cos vars.lineAngle) |> floor) |> toString)
        , y2 (circle.centerY - ((vars.radiusFloat * sin vars.lineAngle) |> floor) |> toString)
        , stroke "black"
        , strokeWidth "4"
        , makeRotateTransform lineTransformAngle { cx = circle.centerX, cy = circle.centerY }
        ]
        []
    ]


type alias Circle =
    { angleOfRotation : AngleOfRotation
    , centerX : Int
    , centerY : Int
    , radius : Int
    , sectionCount : Int
    }


type alias ClockHandVars =
    { centerX : Int
    , centerY : Int
    , lineAngle : Float
    , radiusFloat : Float
    , stringTransformAngle : Float
    , textY : Int
    , trianglePointsString : String
    }


type alias MarkCircle =
    List String -> Circle -> List (Svg.Svg Never)


makeClockHandVars : Circle -> Float -> ClockHandVars
makeClockHandVars circle lineTransformAngle =
    let
        sectionAngle =
            calcSectionAngle circle.sectionCount
    in
    { centerX = circle.centerX
    , centerY = circle.centerY
    , lineAngle = calcLineAngle circle.sectionCount
    , radiusFloat = circle.radius |> toFloat
    , stringTransformAngle = lineTransformAngle - (sectionAngle / 2)
    , textY = circle.centerY - seventyPercentOf circle.radius
    , trianglePointsString = makeTrianglePoints 20 circle
    }


makeWedgeVars : Circle -> Float -> ClockHandVars
makeWedgeVars circle lineTransformAngle =
    let
        clockHandVars =
            makeClockHandVars circle lineTransformAngle
    in
    { clockHandVars | textY = circle.centerY - eightyFivePercentOf circle.radius }


makeRotateTransform : Float -> { cx : Int, cy : Int } -> Attribute Never
makeRotateTransform angle center =
    let
        angleString =
            toString angle

        centerString =
            (center.cx |> toString) ++ " " ++ (center.cy |> toString)

        values =
            angleString ++ " " ++ centerString

        transformValue =
            "rotate(" ++ values ++ ")"
    in
    Svg.Attributes.transform transformValue


calcLineTransformAngle : { a | sectionCount : Int, angleOfRotation : AngleOfRotation } -> Int -> Float
calcLineTransformAngle circle index =
    let
        sectionAngle =
            calcSectionAngle circle.sectionCount
    in
    sectionAngle
        |> (*) (index |> toFloat)
        |> (+) (circle.angleOfRotation |> Angle.inDegrees |> negate)


calcLineAngle : Int -> Float
calcLineAngle circleSectionCount =
    (pi / (circleSectionCount |> toFloat)) + (pi / 2)


calcSectionAngle : Int -> Float
calcSectionAngle circleSectionCount =
    360 / (circleSectionCount |> toFloat)


makeTrianglePoints : Int -> { a | centerX : Int, centerY : Int, radius : Int, angleOfRotation : AngleOfRotation } -> String
makeTrianglePoints sideLength circle =
    let
        topPoint =
            { x = circle.centerX, y = circle.centerY - circle.radius }
    in
    SvgSpace.getEquilateralPointsBasedOnTop sideLength topPoint
        |> List.map stringifyPoint
        |> joinStringsWith " "


type alias Person =
    { name : String
    , job : Maybe String
    }


type alias SvgConfig =
    { class : String
    , fontSize : String
    , width : Int
    , height : Int
    }


type alias Angle =
    Angle.Angle


stringifyPoint : { x : Int, y : Int } -> String
stringifyPoint somePoint =
    toString somePoint.x ++ "," ++ toString somePoint.y


joinStringsWith : String -> List String -> String
joinStringsWith joinString stringList =
    List.foldl (\item aggregate -> aggregate ++ joinString ++ item) "" stringList


seventyPercentOf : Int -> Int
seventyPercentOf someInt =
    0.7
        * (someInt |> toFloat)
        |> floor


eightyFivePercentOf : Int -> Int
eightyFivePercentOf someInt =
    0.85
        * (someInt |> toFloat)
        |> floor

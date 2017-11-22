module WheelView exposing (viewAsClock, viewWheel)

import Html exposing (Html)
import Svg exposing (..)
import SvgSpace
import Angle
import Svg.Attributes exposing (..)

viewAsClock : SvgConfig -> ( List Person, Angle.AngleOfRotation ) -> Html.Html Never
viewAsClock svgConfig ( people, angleOfRotation ) =
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
            , cx = center.x
            , cy = center.y
            , r = (45 * svgConfig.width) // 100
            }

        innerCircle =
            { angleOfRotation = angleOfRotation
            , cx = center.x
            , cy = center.y
            , r = (outerCircle.r * 80) // 100
            }
    in
    svg
        [ svgConfig.width |> toString |> Svg.Attributes.width
        , svgConfig.height |> toString |> Svg.Attributes.height
        ] <|
        List.concat
            [ [ circle
                  [ cx <| (center.x |> toString)
                  , cy <| (center.y |> toString)
                  , fill "tan"
                  , r (outerCircle.r |> toString)
                  ] [ ]
              ]
            , (divideCircle names outerCircle)
            , [ circle
                 [ cx <| (center.x |> toString)
                 , cy <| (center.y |> toString)
                 , fill "lightblue"
                 , r (innerCircle.r |> toString)
                 , stroke "black"
                 , strokeWidth "4"
                 ] [ ]
              ]
            , (drawHands jobs innerCircle)
            ]


drawHands : List String -> { angleOfRotation : Angle.AngleOfRotation , cx : Int, cy: Int, r: Int } -> List (Svg Never)
drawHands strings circle =
    let
        lineAngle =
            (pi / (List.length strings |> toFloat)) + (pi / 2)

        radiusFloat =
            circle.r |> toFloat

        stringY =
            circle.cy - (0.7 * (circle.r |> toFloat) |> floor) |> toString

        angleToTransform : Float -> String
        angleToTransform angle =
            let
                angleString =
                    toString angle

                centerString =
                    (circle.cx |> toString) ++ " " ++ (circle.cy |> toString)

                values =
                    angleString ++ " " ++ centerString
            in
            "rotate(" ++ values ++ ")"

        toElements : Int -> String -> List (Svg Never)
        toElements index someString =
            let
                sectionAngle =
                    360 / (List.length strings |> toFloat)

                angle =
                    sectionAngle
                        |> (*) (index |> toFloat)
                        |> (+) (circle.angleOfRotation |> Angle.inDegrees |> negate)

                transformValue =
                    angle |> angleToTransform

                textTransformValue =
                    (angle - (sectionAngle / 2)) |> angleToTransform

                sideLength =
                    20

                pointsString =
                    SvgSpace.getEquilateralPointsBasedOnTop sideLength { x = circle.cx, y = (circle.cy - circle.r) }
                        |> List.map stringifyPoint
                        |> joinStringsWith " "
            in
            [ text_
                [ x (circle.cx |> toString)
                , y stringY
                , textAnchor "middle"
                , transform textTransformValue
                ]
                [ text someString ]
            , polygon
                [ points pointsString
                , fill "black"
                , transform textTransformValue
                ] [ ]
            , line
                [ x1 (circle.cx |> toString)
                , y1 (circle.cy |> toString)
                , x2 (circle.cx + ((radiusFloat * cos lineAngle) |> floor) |> toString)
                , y2 (circle.cy - ((radiusFloat * sin lineAngle) |> floor) |> toString)
                , stroke "black"
                , strokeWidth "4"
                , transform transformValue
                ] [ ]
            ]
    in
    List.indexedMap toElements strings
        |> List.concat


viewWheel : SvgConfig -> ( List Person, Angle.AngleOfRotation ) -> Html.Html Never
viewWheel svgConfig ( people, angleOfRotation ) =
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
            , cx = center.x
            , cy = center.y
            , r = (45 * svgConfig.width) // 100
            }

        innerCircle =
            { outerCircle
                | r = (outerCircle.r * 80) // 100
                , angleOfRotation = angleOfRotation
            }
    in
    svg
        [ svgConfig.width |> toString |> Svg.Attributes.width
        , svgConfig.height |> toString |> Svg.Attributes.height
        ] <|
        List.concat
            [ [ circle
                  [ cx <| (center.x |> toString)
                  , cy <| (center.y |> toString)
                  , fill "tan"
                  , r (outerCircle.r |> toString)
                  ] [ ]
              ]
            , (divideCircle names outerCircle)
            , [ circle
                 [ cx <| (center.x |> toString)
                 , cy <| (center.y |> toString)
                 , fill "lightblue"
                 , r (innerCircle.r |> toString)
                 , stroke "black"
                 , strokeWidth "4"
                 ] [ ]
              ]
            , (divideCircle jobs innerCircle)
            ]


divideCircle : List String -> { angleOfRotation : Angle.AngleOfRotation , cx : Int, cy: Int, r: Int } -> List (Svg.Svg Never)
divideCircle strings circle =
    let
        lineAngle =
            (pi / (List.length strings |> toFloat)) + (pi / 2)

        radiusFloat =
            circle.r |> toFloat

        stringY =
            circle.cy - (0.85 * (circle.r |> toFloat) |> floor) |> toString

        angleToTransform : Float -> String
        angleToTransform angle =
            let
                angleString =
                    toString angle

                centerString =
                    (circle.cx |> toString) ++ " " ++ (circle.cy |> toString)

                values =
                    angleString ++ " " ++ centerString
            in
            "rotate(" ++ values ++ ")"

        toElements : Int -> String -> List (Svg.Svg Never)
        toElements index someString =
            let
                angle =
                    360 / (List.length strings |> toFloat)
                        |> (*) (index |> toFloat)
                        |> (+) (circle.angleOfRotation |> Angle.inDegrees |> negate)

                transformValue =
                    angle |> angleToTransform
            in
            [ text_
                [ x (circle.cx |> toString)
                , y stringY
                , textAnchor "middle"
                , transform transformValue
                ]
                [ text someString ]
            , line
                [ x1 (circle.cx |> toString)
                , y1 (circle.cy |> toString)
                , x2 (circle.cx + ((radiusFloat * cos lineAngle) |> floor) |> toString)
                , y2 (circle.cy - ((radiusFloat * sin lineAngle) |> floor) |> toString)
                , stroke "black"
                , strokeWidth "4"
                , transform transformValue
                ] [ ]
            ]
    in
    strings
        |> List.indexedMap toElements
        |> List.concat


toSection : SvgConfig -> Person -> Html.Html Never
toSection svgConfig person =
    let
        jobText =
            case person.job of
                Just description ->
                    description

                Nothing ->
                    ""
    in
    text_
        [ x "450"
        , y "450"
        , textAnchor "middle"
        ]
        [ text jobText
        ]
    


type alias Person =
    { name : String
    , job : Maybe String
    }


type alias SvgConfig =
    { width : Int
    , height : Int
    }

type alias Angle =
    Angle.Angle


stringifyPoint : { x : Int, y : Int } -> String
stringifyPoint somePoint =
    (toString somePoint.x) ++ "," ++ (toString somePoint.y)


joinStringsWith : String -> List String -> String
joinStringsWith joinString stringList =
    List.foldl (\item aggregate -> aggregate ++ joinString ++ item) "" stringList

module WheelView exposing (viewWheel)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


viewWheel : SvgConfig -> ( List Person, Float ) -> Html.Html Never
viewWheel svgConfig ( people, angle ) =
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
            { cx = center.x
            , cy = center.y
            , r = (45 * svgConfig.width) // 100
            }

        innerCircle =
            { outerCircle | r = ((outerCircle.r |> toFloat) * 0.8) |> floor }
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
                  , r (0.45 * (svgConfig.width |> toFloat) |> toString)
                  ] [ ]
              ,  circle
                 [ cx <| (center.x |> toString)
                 , cy <| (center.y |> toString)
                 , fill "lightblue"
                 , r (0.35 * (svgConfig.width |> toFloat) |> toString)
                 , stroke "pink"
                 ] [ ]
              ]
            , (divideOuterCircle names outerCircle)
            , (divideInnerCircle jobs innerCircle)
            ]


divideInnerCircle : List String -> { cx : Int, cy: Int, r: Int } -> List (Svg.Svg Never)
divideInnerCircle strings circle =
    let
        lineAngle =
            (pi / (List.length strings |> toFloat)) + (pi / 2)

        radiusFloat =
            circle.r |> toFloat

        stringY =
            circle.cy - (0.8 * (circle.r |> toFloat) |> floor) |> toString

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
                , x2 (circle.cx + ((radiusFloat * cos lineAngle * 0.96) |> floor) |> toString)
                , y2 (circle.cy - ((radiusFloat * sin lineAngle * 0.96) |> floor) |> toString)
                , stroke "black"
                , strokeWidth "4"
                , transform transformValue
                ] [ ]
            ]
    in
    strings
        |> List.indexedMap toElements
        |> List.concat
        

divideOuterCircle : List String -> { cx : Int, cy: Int, r: Int } -> List (Svg.Svg Never)
divideOuterCircle strings circle =
    List.map Svg.text strings


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

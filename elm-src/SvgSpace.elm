module SvgSpace exposing (getEquilateralPointsBasedOnTop)

--in SVG space positive y is in the downward direction

-- for simplicity, implemented only for triangles where the base of the triangle is parallel with the x-axis
getEquilateralPointsBasedOnTop : Int -> Point -> List Point
getEquilateralPointsBasedOnTop sideLength topPoint =
    let
        sideLengthFloat =
            toFloat sideLength

        sharedY =
            topPoint.y + ((sideLengthFloat * sin (60 |> degrees)) |> floor)

        point2 : Point
        point2 =
            { x = topPoint.x - ((sideLengthFloat / 2) |> floor)
            , y = sharedY
            }

        point3 : Point
        point3 = 
            { x = topPoint.x + ((sideLengthFloat / 2) |> floor)
            , y = sharedY
            }
    in
    [ topPoint
    , point2
    , point3
    ]


type alias Point =
    { x : Int
    , y : Int
    }

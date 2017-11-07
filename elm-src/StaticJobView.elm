module StaticJobView exposing (simpleView, Person, SvgConfig)

import Html
import Svg exposing (..)
import Svg.Attributes exposing (x, y)

simpleView : SvgConfig -> List Person -> Html.Html Never
simpleView svgConfig people =
    let
        personX =
            "50"

        responsibilityX =
            150

        peopleCount =
            List.length <| people

        yPositions =
            getPositions peopleCount svgConfig.height

        concatTextElements : ( Int, Person ) -> List (Svg.Svg Never) -> List (Svg.Svg Never)
        concatTextElements ( yValue, person ) svgList =
            let
                personTextElement =
                    text_
                        [ x personX
                        , y <| toString <| yValue
                        ]
                        [ Svg.text person.name ]

                maybeJobTextElement =
                    Maybe.map (viewJob responsibilityX yValue) person.job

                toAppend =
                    case maybeJobTextElement of
                        Just jobTextElement ->
                            [ personTextElement, jobTextElement ]

                        Nothing ->
                            [ personTextElement ]
            in
            List.append svgList toAppend

        textElements =
            people
                |> List.map2 (,) yPositions
                |> List.foldl concatTextElements []
    in
    svg
        [ svgConfig.width |> toString |> Svg.Attributes.width
        , svgConfig.height |> toString |> Svg.Attributes.height
        ]
        textElements


viewJob : Int -> Int -> String -> Svg Never
viewJob xVal yVal job =
    text_
        [ x <| toString <| xVal
        , y <| toString <| yVal
        ]
        [ Svg.text job ]


type alias Person =
    { name : String
    , job : Maybe String
    }


type alias SvgConfig =
    { width : Int
    , height : Int
    }


getPositions : Int -> Int -> List Int
getPositions howMany availableSpace =
    let
        offset =
            availableSpace // (howMany + 1)
    in
    List.range 1 howMany
        |> List.map (\index -> index * offset)

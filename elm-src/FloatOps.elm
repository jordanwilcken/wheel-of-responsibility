module FloatOps exposing (justTheDecimalPart)


justTheDecimalPart : Float -> Float
justTheDecimalPart someFloat =
    let
        negateIfNecessary : Float -> Float
        negateIfNecessary theDecimalPart =
            if someFloat < 0 then
                negate theDecimalPart
            else
                theDecimalPart

        absoluteFloat =
            abs someFloat
    in
    absoluteFloat
        |> floor
        |> toFloat
        |> (-) absoluteFloat
        |> negateIfNecessary

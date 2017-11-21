module Angle exposing (Angle, fromDegrees, fromRadians, inDegrees)


--for simplicity's sake, let's just say that it's degrees under the hood
type Angle =
    Angle Float


fromDegrees : Float -> Angle
fromDegrees someFloat =
    Angle someFloat


fromRadians : Float -> Angle
fromRadians radiansFloat =
    radiansFloat * (360 / (2 * pi))
        |> Angle


inDegrees : Angle -> Float
inDegrees (Angle degrees) =
    degrees

module Angle exposing (Angle, AngleOfRotation, fromDegrees, fromRadians, inDegrees, inRadians)

--for simplicity's sake, let's just say that it's degrees under the hood


type Angle
    = Angle Float



{- What is the difference between an Angle and an AngleOfRotation?
   Angle doesn't concern itself with orientation.
   AngleOfRotation is positive in the counter clockwise direction.
-}


type alias AngleOfRotation =
    Angle


fromDegrees : Float -> Angle
fromDegrees someFloat =
    Angle someFloat


fromRadians : Float -> Angle
fromRadians radiansFloat =
    radiansFloat
        * (360 / (2 * pi))
        |> Angle


inDegrees : Angle -> Float
inDegrees (Angle theDegrees) =
    theDegrees


inRadians : Angle -> Float
inRadians (Angle theDegrees) =
    degrees theDegrees

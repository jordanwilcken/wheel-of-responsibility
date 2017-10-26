port module Ports exposing (loadWheels, saveWheel, timeNow)

import Json.Encode as Encode
import Time


port loadWheels : {} -> Cmd msg


port saveWheel : Encode.Value -> Cmd msg


port timeNow : (Time.Time -> msg) -> Sub msg

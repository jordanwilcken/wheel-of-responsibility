port module Ports exposing (saveWheelCmd, wheels)

import Json.Encode as Encode
import Time


port saveWheelCmd : Encode.Value -> Cmd msg


port wheels : (Encode.Value -> msg) -> Sub msg

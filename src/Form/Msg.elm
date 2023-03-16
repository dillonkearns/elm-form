module Form.Msg exposing (FormData, Method(..), Msg(..))

{-| -}

import Json.Decode


type Msg
    = Submit FormData
    | FormFieldEvent Json.Decode.Value


type alias FormData =
    { fields : List ( String, String )
    , method : Method
    , action : String
    , id : Maybe String
    }


{-| -}
type Method
    = Get
    | Post

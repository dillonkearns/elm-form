module Internal.Field exposing (Field(..), FieldInfo)

{-| -}

import Internal.FieldEvent exposing (FieldEvent)
import Json.Encode as Encode


type Field error parsed input initial kind msg constraints
    = Field (FieldInfo error parsed input initial msg) kind


{-| -}
type alias FieldInfo error parsed input initial msg =
    { initialValue : input -> Maybe String
    , decode : Maybe String -> ( Maybe parsed, List error )
    , properties : List ( String, Encode.Value )
    , initialToString : initial -> String
    , compare : String -> initial -> Order
    , toMsg : Maybe (FieldEvent -> msg)
    }

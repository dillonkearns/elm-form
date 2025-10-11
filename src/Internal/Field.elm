module Internal.Field exposing (EventInfo(..), Field(..), FieldInfo)

{-| -}

import Form.Field.Selection as Selection exposing (Selection)
import Json.Encode as Encode


type Field error parsed input initial kind constraints
    = Field (FieldInfo error parsed input initial) kind


{-| -}
type alias FieldInfo error parsed input initial =
    { initialValue : input -> Maybe String
    , decode : Maybe String -> ( Maybe parsed, List error )
    , properties : List ( String, Encode.Value )
    , initialToString : initial -> String
    , compare : String -> initial -> Order
    , formatOnEvent : Maybe (EventInfo -> Maybe String)
    }


{-| -}
type EventInfo
    = Input Selection
    | Blur String
    | Focus String

module Pages.Internal.Form exposing (EventHandler, Validation(..), ViewField)

import Dict exposing (Dict)
import Form.FieldStatus exposing (FieldStatus)
import Internal.Field
import Json.Encode as Encode


{-| -}
type alias EventHandler =
    Internal.Field.EventInfo -> Maybe String


type Validation error parsed kind field
    = Validation (Maybe (ViewField kind)) (Maybe String) ( Maybe parsed, Dict String (List error) ) (Dict String EventHandler)


{-| -}
type alias ViewField kind =
    { value : Maybe String
    , status : FieldStatus
    , kind : ( kind, List ( String, Encode.Value ) )
    }

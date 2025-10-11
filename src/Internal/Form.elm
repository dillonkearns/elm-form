module Internal.Form exposing (FieldDefinition(..), Form(..))

{-| -}

import Dict exposing (Dict)
import Internal.Field exposing (EventInfo)
import Pages.FormState exposing (FormState)


type Form error combineAndView parsed input
    = Form
        (List ( String, FieldDefinition ))
        (Maybe input
         -> FormState
         ->
            { result : Dict String (List error)
            , isMatchCandidate : Bool
            , combineAndView : combineAndView
            }
        )
        (input -> List ( String, Maybe String ))
        (String -> EventInfo -> Maybe String)


{-| -}
type FieldDefinition
    = RegularField { formatOnEvent : Maybe (EventInfo -> Maybe String) }
    | HiddenField { formatOnEvent : Maybe (EventInfo -> Maybe String) }

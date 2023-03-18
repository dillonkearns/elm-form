module Internal.FieldEvent exposing (..)

{-| -}


type alias FieldEvent =
    { value : String
    , formId : String
    , name : String
    , event : Event
    }


{-| -}
type Event
    = InputEvent String
    | FocusEvent
      --| ChangeEvent
    | BlurEvent

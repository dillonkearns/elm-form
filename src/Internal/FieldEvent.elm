module Internal.FieldEvent exposing (Event(..), FieldEvent, FormData, Method(..), Msg(..))


type alias FieldEvent =
    { value : String
    , formId : String
    , name : String
    , event : Event
    }


type Event
    = InputEvent String
    | FocusEvent
      --| ChangeEvent
    | BlurEvent


type Msg msg
    = Submit FormData (Maybe msg)
    | FormFieldEvent FieldEvent
    | UserMsg msg


type alias FormData =
    { fields : List ( String, String )
    , method : Method
    , action : String
    , id : Maybe String
    }


type Method
    = Get
    | Post

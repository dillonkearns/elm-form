module Internal.FieldEvent exposing (Event(..), FieldEvent, FormData, Method(..), Msg(..), formDataOnSubmit)

import Html
import Html.Events
import Json.Decode as Decode exposing (Decoder)


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
    { fields : Maybe (List ( String, String ))
    , method : Method
    , action : String
    , id : Maybe String
    }


type Method
    = Get
    | Post


formDataOnSubmit : Html.Attribute FormData
formDataOnSubmit =
    Html.Events.preventDefaultOn "submit"
        (Decode.map4
            (\fields method action id ->
                { fields = fields
                , method = method
                , action = action
                , id = id
                }
            )
            fieldsDecoder
            (currentForm "method" methodDecoder)
            (currentForm "action" Decode.string)
            (currentForm "id" (Decode.nullable Decode.string))
            |> Decode.map alwaysPreventDefault
        )


{-| The "fields" property is not standard and requires a JavaScript hack:
<https://github.com/dillonkearns/elm-pages/blob/0cfcd13122448ebfa19e380b332b5a260ec725a2/generator/static-code/elm-pages.js#L107-L117>

The JavaScript snippet above gathers all named HTML form fields and their values as strings.

elm-form does not need to read this property normally, though, since the raw
string values of each form field is tracked in the model. The "fields" property
should be a superset of what we already have in the model. Reading the raw fields
from the event is only needed for an undocumented feature where you can read
the values of named form elements that aren’t created by elm-form.

-}
fieldsDecoder : Decoder (Maybe (List ( String, String )))
fieldsDecoder =
    Decode.maybe
        (Decode.field "fields" tuplesDecoder)


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


tuplesDecoder : Decoder (List ( String, String ))
tuplesDecoder =
    Decode.list
        (Decode.map2 Tuple.pair
            (Decode.index 0 Decode.string)
            (Decode.index 1 Decode.string)
        )


currentForm : String -> Decoder a -> Decoder a
currentForm field_ decoder_ =
    Decode.oneOf
        [ Decode.at [ "submitter", "form" ] decoder_
        , Decode.at [ "currentTarget", field_ ] decoder_
        ]


methodDecoder : Decoder Method
methodDecoder =
    Decode.string
        |> Decode.map
            (\methodString ->
                case methodString |> String.toUpper of
                    "GET" ->
                        Get

                    "POST" ->
                        Post

                    _ ->
                        -- TODO what about "dialog" method? Is it okay for that to be interpreted as GET,
                        -- or should there be a variant for that?
                        Get
            )

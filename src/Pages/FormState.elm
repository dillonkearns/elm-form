module Pages.FormState exposing (FieldState, FormState, listeners)

import Dict exposing (Dict)
import Form.Field.Selection as Selection
import Html exposing (Attribute)
import Html.Attributes as Attr
import Html.Events
import Internal.Field
import Internal.FieldEvent exposing (Event(..), FieldEvent)
import Json.Decode as Decode exposing (Decoder)


{-| -}
listeners : String -> (String -> Internal.Field.EventInfo -> Maybe String) -> List (Attribute FieldEvent)
listeners formId onEventFn =
    [ Html.Events.on "focusin" (fieldEventDecoder onEventFn)
    , Html.Events.on "focusout" (fieldEventDecoder onEventFn)
    , Html.Events.on "input" (fieldEventDecoder onEventFn)
    , Attr.id formId
    ]


{-| -}
fieldEventDecoder : (String -> Internal.Field.EventInfo -> Maybe String) -> Decoder FieldEvent
fieldEventDecoder onEventFn =
    Decode.map4
        (\value formId name selection ->
            { value = value
            , formId = formId
            , name = name
            , selection = selection
            }
        )
        inputValueDecoder
        (Decode.at [ "currentTarget", "id" ] Decode.string)
        (Decode.at [ "target", "name" ] Decode.string
            |> Decode.andThen
                (\name ->
                    if name == "" then
                        Decode.fail "Events only run on fields with names."

                    else
                        Decode.succeed name
                )
        )
        selectionDecoder
        |> Decode.andThen
            (\partial ->
                fieldDecoder
                    |> Decode.map
                        (\event ->
                            let
                                eventInfo : Internal.Field.EventInfo
                                eventInfo =
                                    case event of
                                        InputEvent _ ->
                                            Internal.Field.Input partial.selection

                                        BlurEvent ->
                                            Internal.Field.Blur partial.value

                                        FocusEvent ->
                                            Internal.Field.Focus partial.value

                                newValue : Maybe String
                                newValue =
                                    onEventFn partial.name eventInfo
                            in
                            { value =
                                case newValue of
                                    Just formatted ->
                                        formatted

                                    Nothing ->
                                        partial.value
                            , formId = partial.formId
                            , name = partial.name
                            , event = event
                            }
                        )
            )


{-| Decode selection data (selectionStart and selectionEnd) from the input event.
On blur events, selectionStart might be null, so we default to 0.
-}
selectionDecoder : Decoder Selection.Selection
selectionDecoder =
    Decode.map2 Selection.fromRaw
        (Decode.at [ "target", "value" ] Decode.string)
        (Decode.map2 Tuple.pair
            (Decode.maybe (Decode.at [ "target", "selectionStart" ] Decode.int)
                |> Decode.map (Maybe.withDefault 0)
            )
            (Decode.maybe (Decode.at [ "target", "selectionEnd" ] Decode.int))
        )


{-| -}
inputValueDecoder : Decoder String
inputValueDecoder =
    Decode.at [ "target", "type" ] Decode.string
        |> Decode.andThen
            (\targetType ->
                case targetType of
                    "button" ->
                        Decode.fail "Input and focus events don't run on buttons."

                    "checkbox" ->
                        Decode.map2
                            (\valueWhenChecked isChecked ->
                                if isChecked then
                                    valueWhenChecked

                                else
                                    ""
                            )
                            (Decode.at [ "target", "value" ] Decode.string)
                            (Decode.at [ "target", "checked" ] Decode.bool)

                    _ ->
                        Decode.at [ "target", "value" ] Decode.string
            )


{-| -}
fieldDecoder : Decoder Event
fieldDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "input" ->
                        inputValueDecoder |> Decode.map InputEvent

                    "focusin" ->
                        FocusEvent
                            |> Decode.succeed

                    "focusout" ->
                        BlurEvent
                            |> Decode.succeed

                    _ ->
                        Decode.fail "Unexpected event.type"
            )


{-| -}
type alias FormState =
    { fields : Dict String FieldState
    , submitAttempted : Bool
    }


{-| -}
type alias FieldState =
    { value : String
    , status : Int
    }

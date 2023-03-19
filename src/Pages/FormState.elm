module Pages.FormState exposing (FieldState, FormState, PageFormState, init, listeners, setField, setSubmitAttempted, update)

import Dict exposing (Dict)
import Form.FieldStatus exposing (FieldStatus)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Html.Events
import Internal.FieldEvent exposing (Event(..), FieldEvent)
import Json.Decode as Decode exposing (Decoder)


{-| -}
listeners : String -> List (Attribute FieldEvent)
listeners formId =
    [ Html.Events.on "focusin" fieldEventDecoder
    , Html.Events.on "focusout" fieldEventDecoder
    , Html.Events.on "input" fieldEventDecoder
    , Attr.id formId
    ]


{-| -}
fieldEventDecoder : Decoder FieldEvent
fieldEventDecoder =
    Decode.map4 FieldEvent
        inputValueDecoder
        (Decode.at [ "currentTarget", "id" ] Decode.string)
        (Decode.at [ "target", "name" ] Decode.string)
        fieldDecoder


{-| -}
inputValueDecoder : Decoder String
inputValueDecoder =
    Decode.at [ "target", "type" ] Decode.string
        |> Decode.andThen
            (\targetType ->
                case targetType of
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
update : FieldEvent -> PageFormState -> PageFormState
update fieldEvent pageFormState =
    --if Dict.isEmpty pageFormState then
    --    -- TODO get all initial field values
    --    pageFormState
    --
    --else
    pageFormState
        |> Dict.update fieldEvent.formId
            (\previousValue_ ->
                let
                    previousValue : FormState
                    previousValue =
                        previousValue_
                            |> Maybe.withDefault initSingle
                in
                previousValue
                    |> updateForm fieldEvent
                    |> Just
            )


{-| -}
setField : { formId : String, name : String, value : String } -> PageFormState -> PageFormState
setField info pageFormState =
    pageFormState
        |> Dict.update info.formId
            (\previousValue_ ->
                let
                    previousValue : FormState
                    previousValue =
                        previousValue_
                            |> Maybe.withDefault initSingle
                in
                { previousValue
                    | fields =
                        previousValue.fields
                            |> Dict.update info.name
                                (\previousFieldValue_ ->
                                    let
                                        previousFieldValue : FieldState
                                        previousFieldValue =
                                            previousFieldValue_
                                                |> Maybe.withDefault { value = "", status = Form.FieldStatus.NotVisited }
                                    in
                                    { previousFieldValue | value = info.value }
                                        |> Just
                                )
                }
                    |> Just
            )


{-| -}
updateForm : FieldEvent -> FormState -> FormState
updateForm fieldEvent formState =
    { formState
        | fields =
            formState.fields
                |> Dict.update fieldEvent.name
                    (\previousValue_ ->
                        let
                            previousValue : FieldState
                            previousValue =
                                previousValue_
                                    |> Maybe.withDefault { value = fieldEvent.value, status = Form.FieldStatus.NotVisited }
                        in
                        (case fieldEvent.event of
                            InputEvent newValue ->
                                { previousValue | value = newValue }

                            FocusEvent ->
                                { previousValue | status = previousValue.status |> increaseStatusTo Form.FieldStatus.Focused }

                            BlurEvent ->
                                { previousValue | status = previousValue.status |> increaseStatusTo Form.FieldStatus.Blurred }
                        )
                            |> Just
                    )
    }


{-| -}
setSubmitAttempted : String -> PageFormState -> PageFormState
setSubmitAttempted fieldId pageFormState =
    pageFormState
        |> Dict.update fieldId
            (\maybeForm ->
                case maybeForm of
                    Just formState ->
                        Just { formState | submitAttempted = True }

                    Nothing ->
                        Just { initSingle | submitAttempted = True }
            )


{-| -}
initSingle : FormState
initSingle =
    { fields = Dict.empty
    , submitAttempted = False
    }


init : PageFormState
init =
    Dict.empty


{-| -}
type alias PageFormState =
    Dict String FormState


{-| -}
type alias FormState =
    { fields : Dict String FieldState
    , submitAttempted : Bool
    }


{-| -}
type alias FieldState =
    { value : String
    , status : FieldStatus
    }


{-| -}
increaseStatusTo : FieldStatus -> FieldStatus -> FieldStatus
increaseStatusTo increaseTo currentStatus =
    if statusRank increaseTo > statusRank currentStatus then
        increaseTo

    else
        currentStatus


{-| -}
statusRank : FieldStatus -> Int
statusRank status =
    case status of
        Form.FieldStatus.NotVisited ->
            0

        Form.FieldStatus.Focused ->
            1

        Form.FieldStatus.Changed ->
            2

        Form.FieldStatus.Blurred ->
            3

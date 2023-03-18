module Form.State exposing
    ( Msg(..), FormData, Method(..), onSubmitDecoder
    , init, update
    , FieldStatus(..), fieldStatusToString
    )

{-| `elm-form` manages the client-side state of fields, including FieldStatus which you can use to determine when
in the user's workflow to show validation errors.

@docs Msg, FormData, Method, onSubmitDecoder

@docs init, update


## Field Status

@docs FieldStatus, fieldStatusToString

-}

-- TODO merge this with `FormState` module and expose there?

import Dict exposing (Dict)
import Internal.FieldEvent exposing (Event(..), FieldEvent)
import Json.Decode as Decode exposing (Decoder)
import Task


{-| -}
onSubmitDecoder : Decoder (Msg msg)
onSubmitDecoder =
    Decode.map4 FormData
        (Decode.field "fields"
            (Decode.list
                (Decode.map2 Tuple.pair
                    (Decode.index 0 Decode.string)
                    (Decode.index 1 Decode.string)
                )
            )
        )
        (Decode.field "method" methodDecoder)
        (Decode.field "action" Decode.string)
        (Decode.field "id" (Decode.nullable Decode.string))
        |> Decode.map (\thing -> Submit thing Nothing)


methodDecoder : Decoder Method
methodDecoder =
    Decode.string
        |> Decode.andThen
            (\method ->
                case method |> String.toUpper of
                    "GET" ->
                        Decode.succeed Get

                    _ ->
                        Decode.succeed Post
            )


{-| -}
type Msg msg
    = Submit FormData (Maybe msg)
    | FormFieldEvent FieldEvent
    | UserMsg msg


{-| -}
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


{-| -}
update : Msg msg -> State -> ( State, Cmd msg )
update formMsg formModel =
    case formMsg of
        UserMsg myMsg ->
            ( formModel
            , Task.succeed myMsg |> Task.perform identity
            )

        FormFieldEvent value ->
            ( updateInternal value formModel
            , Cmd.none
            )

        Submit formData maybeMsg ->
            ( setSubmitAttempted
                (formData.id |> Maybe.withDefault "form")
                formModel
            , maybeMsg
                |> Maybe.map (\userMsg -> Task.succeed userMsg |> Task.perform identity)
                |> Maybe.withDefault Cmd.none
            )


{-| -}
updateInternal : FieldEvent -> State -> State
updateInternal fieldEvent pageFormState =
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
                                    |> Maybe.withDefault { value = fieldEvent.value, status = NotVisited }
                        in
                        (case fieldEvent.event of
                            InputEvent newValue ->
                                { previousValue | value = newValue }

                            FocusEvent ->
                                { previousValue | status = previousValue.status |> increaseStatusTo Focused }

                            BlurEvent ->
                                { previousValue | status = previousValue.status |> increaseStatusTo Blurred }
                        )
                            |> Just
                    )
    }


{-| -}
init : State
init =
    Dict.empty


{-| -}
type FieldStatus
    = NotVisited
    | Focused
    | Changed
    | Blurred


{-| -}
fieldStatusToString : FieldStatus -> String
fieldStatusToString fieldStatus =
    case fieldStatus of
        NotVisited ->
            "NotVisited"

        Focused ->
            "Focused"

        Changed ->
            "Changed"

        Blurred ->
            "Blurred"


setSubmitAttempted : String -> State -> State
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
type alias State =
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
initSingle : FormState
initSingle =
    { fields = Dict.empty
    , submitAttempted = False
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
        NotVisited ->
            0

        Focused ->
            1

        Changed ->
            2

        Blurred ->
            3

module Form.State exposing
    ( Msg(..), FormData, Method(..), onSubmitDecoder
    , init, update
    )

{-|

@docs Msg, FormData, Method, onSubmitDecoder

@docs init, update

-}

-- TODO merge this with `FormState` module and expose there?

import Dict
import Json.Decode as Decode exposing (Decoder)
import Pages.FormState exposing (FieldEvent)
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
update : Msg msg -> Pages.FormState.PageFormState -> ( Pages.FormState.PageFormState, Cmd msg )
update formMsg formModel =
    case formMsg of
        UserMsg myMsg ->
            ( formModel
            , Task.succeed myMsg |> Task.perform identity
            )

        FormFieldEvent value ->
            ( Pages.FormState.update value formModel
            , Cmd.none
            )

        Submit formData maybeMsg ->
            ( Pages.FormState.setSubmitAttempted
                (formData.id |> Maybe.withDefault "form")
                formModel
            , maybeMsg
                |> Maybe.map (\userMsg -> Task.succeed userMsg |> Task.perform identity)
                |> Maybe.withDefault Cmd.none
            )


{-| -}
init : Pages.FormState.PageFormState
init =
    Dict.empty

module Form.Msg exposing (Msg(..), FormData, Method(..), onSubmitDecoder)

{-|

@docs Msg, FormData, Method, onSubmitDecoder

-}

import Json.Decode as Decode exposing (Decoder)
import Pages.FormState exposing (FieldEvent)


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
        |> Decode.map Submit


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
    = Submit FormData
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

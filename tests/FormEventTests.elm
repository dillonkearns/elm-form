module FormEventTests exposing (all)

import Dict
import Expect
import Form
import Form.Field as Field
import Form.Validation as Validation
import Html
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query


type Msg
    = FormMsg (Form.Msg Msg)


formWithField : String -> Form.HtmlForm String ( String, String ) input Msg
formWithField name =
    Form.form
        (\id_ name_ ->
            { combine =
                Validation.succeed Tuple.pair
                    |> Validation.andMap id_
                    |> Validation.andMap name_
            , view = \_ -> []
            }
        )
        |> Form.field name (Field.text |> Field.required "Required")
        |> Form.field "name" (Field.text |> Field.required "Required")


renderForm : String -> Html.Html Msg
renderForm name =
    formWithField name
        |> Form.renderHtml
            { submitting = False
            , state = Form.init
            , toMsg = FormMsg
            }
            (Form.options "myForm")
            []


inputEvent : Encode.Value -> ( String, Encode.Value )
inputEvent idValue =
    ( "input"
    , Encode.object
        [ ( "type", Encode.string "input" )
        , ( "target"
          , Encode.object
                [ ( "type", Encode.string "text" )
                , ( "value", Encode.string "hello" )
                , ( "name", Encode.string "name" )
                ]
          )
        , ( "currentTarget"
          , Encode.object [ ( "id", idValue ) ]
          )
        ]
    )


shadowedIdElement : Encode.Value
shadowedIdElement =
    Encode.object
        [ ( "tagName", Encode.string "INPUT" )
        , ( "name", Encode.string "id" )
        ]


updatedNameValue : Result String Msg -> Maybe String
updatedNameValue result =
    case result of
        Ok (FormMsg formMsg) ->
            Form.updateWithMsg formMsg Form.init
                |> Tuple.first
                |> Dict.get "myForm"
                |> Maybe.andThen (.fields >> Dict.get "name")
                |> Maybe.map .value

        _ ->
            Nothing


all : Test
all =
    describe "a field named \"id\" shadows form.id"
        [ test "control: input event on a form with a non-reserved field name works" <|
            \() ->
                renderForm "record-id"
                    |> Query.fromHtml
                    |> Event.simulate (inputEvent (Encode.string "myForm"))
                    |> Event.toResult
                    |> updatedNameValue
                    |> Expect.equal (Just "hello")
        , test "input event on a form with a field named \"id\" still updates form state" <|
            \() ->
                renderForm "id"
                    |> Query.fromHtml
                    |> Event.simulate (inputEvent shadowedIdElement)
                    |> Event.toResult
                    |> updatedNameValue
                    |> Expect.equal (Just "hello")
        ]

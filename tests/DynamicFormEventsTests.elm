module DynamicFormEventsTests exposing (all)

import Dict
import Expect
import Form
import Form.Field as Field
import Form.Validation as Validation
import Internal.FieldEvent exposing (Event(..), FieldEvent, Msg(..))
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Dynamic Form with formatOnEvent"
        [
            test "formatOnEvent is triggered on blur in a dynamic form" <|
            \() ->
                let
                    formId =
                        "dyn-form"

                    postForm =
                        Form.form
                            (\title ->
                                {
                                    combine = Validation.succeed identity |> Validation.andMap title,
                                    view = \_ -> []
                                }
                            )
                            |> Form.field "title"
                                (Field.text
                                    |> Field.formatOnEvent
                                        (\event ->
                                            case event of
                                                Field.Blur value ->
                                                    Just (String.trim value)

                                                _ ->
                                                    Nothing
                                        )
                                    |> Field.required "Required"
                                )

                    dynamicForm =
                        Form.form
                            (\kind postForm_ ->
                                {
                                    combine = kind |> Validation.andThen postForm_.combine,
                                    view = \_ -> []
                                }
                            )
                            |> Form.field "kind"
                                (Field.select
                                    [ ( "post", Post ) ]
                                    (\ _ -> "Invalid")
                                    |> Field.required "Required"
                                )
                            |> Form.dynamic
                                (\parsedKind ->
                                    case parsedKind of
                                        Post -> postForm
                                )

                    initialState =
                        Form.init |> Dict.insert formId { fields = Dict.empty, submitAttempted = False }

                    ( stateAfterKind, _ ) = Form.update (FormFieldEvent { value = "post", formId = formId, name = "kind", event = InputEvent "post" }) initialState

                    ( stateAfterTitleInput, _ ) = Form.update (FormFieldEvent { value = "  my title  ", formId = formId, name = "title", event = InputEvent "  my title  " }) stateAfterKind

                    ( stateAfterBlur, _ ) = Form.update (FormFieldEvent { value = "  my title  ", formId = formId, name = "title", event = BlurEvent }) stateAfterTitleInput

                    validated = Form.parse formId stateAfterBlur () dynamicForm
                in
                case validated of
                    Form.Valid parsed ->
                        Expect.equal "my title" parsed

                    Form.Invalid _ errors ->
                        Expect.fail ("Expected form to be valid, but it was invalid with errors: " ++ Debug.toString errors)
        ]


type PostKind
    = Post
module Main exposing (main)

import Browser
import Form
import Form.Field as Field
import Form.FieldView as FieldView
import Form.Validation as Validation
import Html exposing (Html, div)
import Html.Attributes
import Username exposing (Username)


type Msg
    = FormMsg (Form.Msg Msg)
    | OnSubmit
        { fields : List ( String, String )
        , method : Form.Method
        , action : String
        , parsed : Form.Validated String SignUpForm
        }


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { formState : Form.Model
    , submitting : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { formState = Form.init
      , submitting = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnSubmit parsed ->
            let
                _ =
                    Debug.log "OnSubmit!" parsed
            in
            ( model, Cmd.none )

        FormMsg formMsg ->
            let
                ( updatedFormModel, cmd ) =
                    Form.update formMsg model.formState
            in
            ( { model | formState = updatedFormModel }, cmd )


view : Model -> Browser.Document Msg
view model =
    { title = "elm-form demo"
    , body =
        [ div []
            [ signUpForm
                |> Form.renderHtml
                    { submitting = model.submitting
                    , state = model.formState
                    , toMsg = FormMsg
                    }
                    (Form.options "form"
                        |> Form.withOnSubmit OnSubmit
                    )
                    []
            ]
        ]
    }


signUpForm : Form.HtmlForm String SignUpForm input msg
signUpForm =
    (\username password passwordConfirmation chaos time role ->
        { combine =
            Validation.succeed SignUpForm
                |> Validation.andMap
                    (username
                        |> Validation.map Username.fromString
                        |> Validation.fromResult
                    )
                |> Validation.andMap
                    (Validation.map2
                        (\passwordValue passwordConfirmationValue ->
                            if passwordValue == passwordConfirmationValue then
                                Validation.succeed passwordValue

                            else
                                Validation.fail "Must match password" passwordConfirmation
                        )
                        password
                        passwordConfirmation
                        |> Validation.andThen identity
                    )
                |> Validation.andMap role
        , view =
            \formState ->
                let
                    fieldView label field =
                        Html.div []
                            [ Html.label []
                                [ Html.text (label ++ " ")
                                , FieldView.input [] field
                                , Validation.fieldStatus field |> Validation.fieldStatusToString |> Html.text
                                , errorsView formState field
                                ]
                            ]
                in
                [ fieldView "username" username
                , fieldView "Password" password
                , fieldView "Password Confirmation" passwordConfirmation
                , fieldView "Chaos" chaos
                , fieldView "Time" time
                , Html.div []
                    [ FieldView.select []
                        (\entry -> ( [], roleToString entry ))
                        role
                    ]
                , if formState.submitting then
                    Html.button
                        [ Html.Attributes.disabled True ]
                        [ Html.text "Signing Up..." ]

                  else
                    Html.button [] [ Html.text "Sign Up" ]
                ]
        }
    )
        |> Form.form
        |> Form.field "username"
            (Field.text
                |> Field.required "Required"
                |> Field.withInitialValue (\_ -> "dillon")
            )
        |> Form.field "password" (Field.text |> Field.password |> Field.required "Required")
        |> Form.field "password-confirmation" (Field.text |> Field.password |> Field.required "Required")
        |> Form.field "chaos"
            (Field.int { invalid = \_ -> "Invalid" }
                |> Field.range
                    { invalid = \_ -> "Outside of range"
                    , missing = "Required"
                    , min = 1
                    , max = 100
                    }
                |> Field.withInitialValue (\_ -> 10)
            )
        |> Form.field "time"
            (Field.time { invalid = \_ -> "Invalid" }
                |> Field.withMin
                    { hours = 10
                    , minutes = 0
                    , seconds = Nothing
                    }
                    "Must be after 10"
                |> Field.withMax
                    { hours = 12
                    , minutes = 0
                    , seconds = Nothing
                    }
                    "Must be before noon"
            )
        |> Form.field "role"
            (Field.select
                [ ( "admin", Admin )
                , ( "super-admin", SuperAdmin )
                , ( "regular", Regular )
                ]
                (\_ -> "Invalid")
                |> Field.required "Required"
                |> Field.withInitialValue (\_ -> Admin)
            )


roleToString : Role -> String
roleToString role =
    case role of
        SuperAdmin ->
            "SuperAdmin"

        Admin ->
            "Admin"

        Regular ->
            "Regular"


type Role
    = SuperAdmin
    | Admin
    | Regular


type alias SignUpForm =
    { username : Username, password : String, role : Role }


errorsView :
    Form.Context String input
    -> Validation.Field String parsed kind
    -> Html msg
errorsView { submitAttempted, errors } field =
    if submitAttempted || Validation.statusAtLeast Validation.Blurred field then
        -- only show validations when a field has been blurred
        -- (it can be annoying to see errors while you type the initial entry for a field, but we want to see the current
        -- errors once we've left the field, even if we are changing it so we know once it's been fixed or whether a new
        -- error is introduced)
        errors
            |> Form.errorsForField field
            |> List.map (\error -> Html.li [ Html.Attributes.style "color" "red" ] [ Html.text error ])
            |> Html.ul []

    else
        Html.ul [] []

module Main exposing (main)

import Browser
import Form
import Form.Field as Field
import Form.FieldView as FieldView
import Form.State
import Form.Validation as Validation
import Html exposing (Html, div)
import Html.Attributes
import Username exposing (Username)


type Msg
    = FormMsg (Form.State.Msg Msg)
    | OnSubmit { fields : List ( String, String ), parsed : Result () SignUpForm }


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
    { formState : Form.State.State
    , isTransitioning : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { formState = Form.State.init
      , isTransitioning = False
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
                    Form.State.update formMsg model.formState
            in
            ( { model | formState = updatedFormModel }, cmd )


view : Model -> Browser.Document Msg
view model =
    { title = "elm-form demo"
    , body =
        [ div []
            [ signUpForm
                |> Form.withOnSubmit OnSubmit
                |> Form.renderHtml "form"
                    []
                    -- TODO get rid of errorData argument (completely, or just for vanilla apps)
                    (\_ -> Nothing)
                    { isTransitioning = model.isTransitioning
                    , state = model.formState
                    }
                    ()
                |> Html.map FormMsg
            ]
        ]
    }


signUpForm : Form.HtmlForm String SignUpForm input msg
signUpForm =
    (\username password passwordConfirmation ->
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
        , view =
            \formState ->
                let
                    fieldView label field =
                        Html.div []
                            [ Html.label []
                                [ Html.text (label ++ " ")
                                , FieldView.input [] field
                                , errorsView formState field
                                ]
                            ]
                in
                [ fieldView "username" username
                , fieldView "Password" password
                , fieldView "Password Confirmation" passwordConfirmation
                , if formState.isTransitioning then
                    Html.button
                        [ Html.Attributes.disabled True ]
                        [ Html.text "Signing Up..." ]

                  else
                    Html.button [] [ Html.text "Sign Up" ]
                ]
        }
    )
        |> Form.init
        |> Form.field "username" (Field.text |> Field.required "Required")
        |> Form.field "password" (Field.text |> Field.password |> Field.required "Required")
        |> Form.field "password-confirmation" (Field.text |> Field.password |> Field.required "Required")


type alias SignUpForm =
    { username : Username, password : String }


errorsView :
    Form.Context String input
    -> Validation.Field String parsed kind
    -> Html msg
errorsView { submitAttempted, errors } field =
    if submitAttempted || Validation.statusAtLeast Form.State.Blurred field then
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

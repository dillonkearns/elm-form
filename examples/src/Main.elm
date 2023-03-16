module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Form
import Form.Field as Field
import Form.FieldView as FieldView
import Form.Msg
import Form.Validation as Validation
import Html exposing (Html, div)
import Html.Attributes
import Pages.FormState


type Msg
    = FormMsg (Form.Msg.Msg Msg)
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
    { pageFormState : Pages.FormState.PageFormState
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { pageFormState = Dict.empty
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
            case formMsg of
                Form.Msg.UserMsg myMsg ->
                    update myMsg model

                Form.Msg.FormFieldEvent value ->
                    ( { model
                        | pageFormState = Pages.FormState.update value model.pageFormState
                      }
                    , Cmd.none
                    )

                Form.Msg.Submit formData ->
                    ( --{ model
                      --    | pageFormState = Pages.FormState.update value model.pageFormState
                      --  }
                      model
                    , Cmd.none
                    )


view : Model -> Browser.Document Msg
view model =
    { title = "elm-form demo"
    , body =
        [ div []
            [ signUpForm
                |> Form.withOnSubmit OnSubmit
                |> Form.renderHtml "form"
                    []
                    (\_ -> Nothing)
                    { path = []
                    , action = Nothing -- Maybe actionData

                    --, submit :
                    --    { fields : List ( String, String ), headers : List ( String, String ) }
                    --    -> Pages.Fetcher.Fetcher (Result Http.Error action)
                    , transition = Nothing --Maybe Transition
                    , fetchers = Dict.empty --Dict String (Pages.Transition.FetcherState (Maybe actionData))
                    , pageFormState = model.pageFormState
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
                |> Validation.andMap username
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
                            [ Html.label
                                []
                                [ Html.text (label ++ " ")
                                , FieldView.input [] field
                                , errorsView formState.errors field
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
        |> Form.hiddenKind ( "kind", "regular" ) "Expected kind."
        |> Form.field "username" (Field.text |> Field.required "Required")
        |> Form.field "password" (Field.text |> Field.password |> Field.required "Required")
        |> Form.field "password-confirmation" (Field.text |> Field.password |> Field.required "Required")


type alias SignUpForm =
    { username : String, password : String }


errorsView :
    Form.Errors String
    -> Validation.Field String parsed kind
    -> Html msg
errorsView errors field =
    errors
        |> Form.errorsForField field
        |> List.map (\error -> Html.li [ Html.Attributes.style "color" "red" ] [ Html.text error ])
        |> Html.ul []

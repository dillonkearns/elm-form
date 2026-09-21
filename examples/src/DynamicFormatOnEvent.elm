module DynamicFormatOnEvent exposing (Model, Msg, init, update, view)

import Form
import Form.Field as Field
import Form.Field.Selection as Selection
import Form.FieldView as FieldView
import Form.Validation as Validation
import Html exposing (Html)
import Html.Attributes as Attr


type Msg
    = FormMsg (Form.Msg Msg)
    | OnSubmit
        { fields : List ( String, String )
        , method : Form.Method
        , action : String
        , parsed : Form.Validated String ContactInfo
        }


type alias Model =
    { formState : Form.Model
    , submitting : Bool
    , lastSubmission : Maybe (Form.Validated String ContactInfo)
    }


init : Model
init =
    { formState = Form.init
    , submitting = False
    , lastSubmission = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnSubmit { parsed } ->
            let
                _ =
                    Debug.log "Submitted" parsed
            in
            ( { model | lastSubmission = Just parsed }, Cmd.none )

        FormMsg formMsg ->
            let
                ( updatedFormModel, cmd ) =
                    Form.update formMsg model.formState
            in
            ( { model | formState = updatedFormModel }, cmd )


view : Model -> Html Msg
view model =
    Html.div [ Attr.style "max-width" "600px", Attr.style "margin" "2rem auto", Attr.style "font-family" "sans-serif" ]
        [ Html.h2 [] [ Html.text "Dynamic Form with formatOnEvent" ]
        , Html.p [ Attr.style "color" "#666" ]
            [ Html.text "Select a contact type, then fill in the fields. The phone field formats on blur (adds dashes). The email field lowercases on blur." ]
        , contactForm
            |> Form.renderHtml
                { submitting = model.submitting
                , state = model.formState
                , toMsg = FormMsg
                }
                (Form.options "dynamic-format"
                    |> Form.withOnSubmit OnSubmit
                )
                [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "1rem" ]
        , case model.lastSubmission of
            Just (Form.Valid value) ->
                Html.div [ Attr.style "margin-top" "1rem", Attr.style "padding" "1rem", Attr.style "background" "#e8f5e9" ]
                    [ Html.text ("Submitted: " ++ contactInfoToString value) ]

            Just (Form.Invalid _ errors) ->
                Html.div [ Attr.style "margin-top" "1rem", Attr.style "padding" "1rem", Attr.style "background" "#fce4ec" ]
                    [ Html.text ("Invalid: " ++ Debug.toString errors) ]

            Nothing ->
                Html.text ""
        ]



-- TYPES


type ContactType
    = Phone
    | Email


type ContactInfo
    = PhoneContact { phone : String }
    | EmailContact { email : String }


contactInfoToString : ContactInfo -> String
contactInfoToString info =
    case info of
        PhoneContact { phone } ->
            "Phone: " ++ phone

        EmailContact { email } ->
            "Email: " ++ email



-- FORMS


contactForm : Form.HtmlForm String ContactInfo () Msg
contactForm =
    (\contactType subForm ->
        { combine =
            contactType
                |> Validation.andThen subForm.combine
        , view =
            \formState ->
                [ Html.div []
                    [ Html.label []
                        [ Html.text "Contact Type "
                        , FieldView.select [] (\entry -> ( [], contactTypeToString entry )) contactType
                        ]
                    ]
                , case Validation.value contactType of
                    Just ct ->
                        Html.div [] (subForm.view ct formState)

                    Nothing ->
                        Html.text "Select a contact type above"
                , Html.button [ Attr.style "padding" "0.5rem 1rem" ] [ Html.text "Submit" ]
                ]
        }
    )
        |> Form.form
        |> Form.field "contactType"
            (Field.select
                [ ( "phone", Phone )
                , ( "email", Email )
                ]
                (\_ -> "Invalid contact type")
                |> Field.required "Please select a contact type"
            )
        |> Form.dynamic
            (\parsedType ->
                case parsedType of
                    Phone ->
                        phoneSubForm

                    Email ->
                        emailSubForm
            )


phoneSubForm : Form.HtmlForm String ContactInfo () Msg
phoneSubForm =
    (\phone ->
        { combine =
            phone
                |> Validation.map (\phoneValue -> PhoneContact { phone = phoneValue })
        , view =
            \formState ->
                [ inputFieldView formState "Phone (formats on blur: 1234567890 → 123-456-7890)" phone
                ]
        }
    )
        |> Form.form
        |> Form.field "phone"
            (Field.text
                |> Field.required "Phone is required"
                |> Field.telephone
                |> Field.formatOnEvent
                    (\event ->
                        case event of
                            Field.Blur value ->
                                let
                                    digits =
                                        String.filter Char.isDigit value
                                in
                                if String.length digits == 10 then
                                    Just
                                        (String.slice 0 3 digits
                                            ++ "-"
                                            ++ String.slice 3 6 digits
                                            ++ "-"
                                            ++ String.slice 6 10 digits
                                        )

                                else
                                    Nothing

                            Field.Input selection ->
                                let
                                    currentValue =
                                        Selection.value selection

                                    digitsOnly =
                                        String.filter Char.isDigit currentValue
                                in
                                if digitsOnly /= currentValue then
                                    Just digitsOnly

                                else
                                    Nothing

                            Field.Focus _ ->
                                Nothing
                    )
            )


emailSubForm : Form.HtmlForm String ContactInfo () Msg
emailSubForm =
    (\email ->
        { combine =
            email
                |> Validation.map (\emailValue -> EmailContact { email = emailValue })
        , view =
            \formState ->
                [ inputFieldView formState "Email (lowercases on blur)" email
                ]
        }
    )
        |> Form.form
        |> Form.field "email"
            (Field.text
                |> Field.required "Email is required"
                |> Field.email
                |> Field.formatOnEvent
                    (\event ->
                        case event of
                            Field.Blur value ->
                                Just (String.toLower value)

                            _ ->
                                Nothing
                    )
            )



-- HELPERS


contactTypeToString : ContactType -> String
contactTypeToString ct =
    case ct of
        Phone ->
            "Phone"

        Email ->
            "Email"


inputFieldView : Form.Context String input -> String -> Validation.Field String parsed FieldView.Input -> Html msg
inputFieldView formState label field =
    Html.div []
        [ Html.label []
            [ Html.text (label ++ " ")
            , FieldView.input [ Attr.style "padding" "0.25rem" ] field
            ]
        , errorsView formState field
        ]


errorsView : Form.Context String input -> Validation.Field String parsed kind -> Html msg
errorsView { submitAttempted, errors } field =
    if submitAttempted || Validation.statusAtLeast Validation.Blurred field then
        errors
            |> Form.errorsForField field
            |> List.map (\error -> Html.li [ Attr.style "color" "red" ] [ Html.text error ])
            |> Html.ul []

    else
        Html.ul [] []

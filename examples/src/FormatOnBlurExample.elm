module FormatOnBlurExample exposing (main)

import Browser
import Form
import Form.Field as Field exposing (EventInfo(..))
import Form.Field.Selection as Selection
import Form.FieldView as FieldView
import Form.Validation as Validation
import Html exposing (Html)
import Html.Attributes as Attr


type Msg
    = FormMsg (Form.Msg Msg)
    | OnSubmit (Form.Validated String ContactForm)


type alias Model =
    { formState : Form.Model
    , submitting : Bool
    }


type alias ContactForm =
    { name : String
    , email : String
    , phone : String
    , code : String
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { formState = Form.init
      , submitting = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnSubmit validated ->
            case validated of
                Form.Valid data ->
                    let
                        _ =
                            Debug.log "Valid form data" data
                    in
                    ( { model | submitting = False }, Cmd.none )

                Form.Invalid _ errors ->
                    let
                        _ =
                            Debug.log "Form errors" errors
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
    { title = "formatOnEvent Example"
    , body =
        [ Html.div [ Attr.style "max-width" "600px", Attr.style "margin" "40px auto", Attr.style "font-family" "sans-serif" ]
            [ Html.h1 [] [ Html.text "formatOnEvent Example" ]
            , Html.p []
                [ Html.text "This example demonstrates the "
                , Html.code [] [ Html.text "formatOnEvent" ]
                , Html.text " feature with different event types:"
                ]
            , Html.ul []
                [ Html.li [] [ Html.text "Name, Email, Phone: formatted on blur only" ]
                , Html.li [] [ Html.text "Code: formatted on input (when cursor at end) and blur" ]
                ]
            , Html.p []
                [ Html.text "Try typing in the Code field - it will uppercase as you type, but only if your cursor is at the end. Try moving the cursor to the middle and typing to see the difference!" ]
            , contactForm
                |> Form.renderHtml
                    { submitting = model.submitting
                    , state = model.formState
                    , toMsg = FormMsg
                    }
                    (Form.options "contact-form"
                        |> Form.withOnSubmit (\{ parsed } -> OnSubmit parsed)
                    )
                    []
            ]
        ]
    }


contactForm : Form.HtmlForm String ContactForm input msg
contactForm =
    (\name email phone code ->
        { combine =
            Validation.succeed ContactForm
                |> Validation.andMap name
                |> Validation.andMap email
                |> Validation.andMap phone
                |> Validation.andMap code
        , view =
            \formState ->
                [ fieldView formState "Name (trimmed on blur)" name
                , fieldView formState "Email (trimmed and lowercased on blur)" email
                , fieldView formState "Phone (formatted on blur)" phone
                , fieldView formState "Code (uppercased as you type, only at end)" code
                , Html.button
                    [ Attr.style "margin-top" "20px"
                    , Attr.style "padding" "10px 20px"
                    ]
                    [ if formState.submitting then
                        Html.text "Submitting..."

                      else
                        Html.text "Submit"
                    ]
                ]
        }
    )
        |> Form.form
        |> Form.field "name"
            (Field.text
                |> Field.formatOnEvent
                    (\event ->
                        case event of
                            Blur value ->
                                Just (String.trim value)

                            _ ->
                                Nothing
                    )
                |> Field.required "Name is required"
            )
        |> Form.field "email"
            (Field.text
                |> Field.formatOnEvent
                    (\event ->
                        case event of
                            Blur value ->
                                Just (String.trim value |> String.toLower)

                            _ ->
                                Nothing
                    )
                |> Field.required "Email is required"
                |> Field.email
            )
        |> Form.field "phone"
            (Field.text
                |> Field.formatOnEvent
                    (\event ->
                        case event of
                            Blur value ->
                                Just (formatPhoneNumber value)

                            _ ->
                                Nothing
                    )
                |> Field.required "Phone is required"
            )
        |> Form.field "code"
            (Field.text
                |> Field.formatOnEvent
                    (\event ->
                        case event of
                            Input selection ->
                                -- Only format if cursor is at the end
                                if Selection.cursorAtEnd selection then
                                    Just (Selection.value selection |> String.toUpper)

                                else
                                    Nothing

                            Blur value ->
                                -- Always format on blur
                                Just (String.toUpper value)

                            _ ->
                                Nothing
                    )
                |> Field.required "Code is required"
            )


fieldView :
    Form.Context String input
    -> String
    -> Validation.Field String parsed FieldView.Input
    -> Html msg
fieldView formState label field =
    Html.div [ Attr.style "margin-bottom" "20px" ]
        [ Html.label [ Attr.style "display" "block", Attr.style "margin-bottom" "5px", Attr.style "font-weight" "bold" ]
            [ Html.text label ]
        , FieldView.input
            [ Attr.style "width" "100%"
            , Attr.style "padding" "8px"
            , Attr.style "font-size" "16px"
            , Attr.style "border" "1px solid #ccc"
            , Attr.style "border-radius" "4px"
            ]
            field
        , errorsView formState field
        ]


errorsView :
    Form.Context String input
    -> Validation.Field String parsed FieldView.Input
    -> Html msg
errorsView formState field =
    if formState.submitAttempted || Validation.statusAtLeast Validation.Blurred field then
        formState.errors
            |> Form.errorsForField field
            |> List.map
                (\error ->
                    Html.div
                        [ Attr.style "color" "red"
                        , Attr.style "margin-top" "5px"
                        , Attr.style "font-size" "14px"
                        ]
                        [ Html.text error ]
                )
            |> Html.div []

    else
        Html.text ""


{-| Simple phone number formatter: removes non-digits and formats as (XXX) XXX-XXXX
-}
formatPhoneNumber : String -> String
formatPhoneNumber input =
    let
        digits =
            input
                |> String.filter Char.isDigit
                |> String.left 10
    in
    case String.length digits of
        10 ->
            "("
                ++ String.left 3 digits
                ++ ") "
                ++ String.slice 3 6 digits
                ++ "-"
                ++ String.dropLeft 6 digits

        7 ->
            String.left 3 digits ++ "-" ++ String.dropLeft 3 digits

        _ ->
            digits

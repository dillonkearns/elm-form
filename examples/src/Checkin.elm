module Checkin exposing (main)

import Browser
import Date exposing (Date)
import Form
import Form.Field as Field exposing (TimeOfDay)
import Form.FieldView as FieldView
import Form.Validation as Validation
import Html exposing (Html, div)
import Html.Attributes


type Msg
    = FormMsg (Form.Msg Msg)
    | OnSubmit
        { fields : List ( String, String )
        , method : Form.Method
        , action : String
        , parsed : Form.Validated String Stay
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
            [ example
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


type alias Stay =
    { name : String
    , checkIn : Checkin
    , emailUpdates : Bool
    }


type alias Checkin =
    { date : Date
    , nights : Int
    , time : TimeOfDay
    }


example : Form.HtmlForm String Stay input Msg
example =
    (\name checkIn checkOut checkInTime emailUpdates ->
        { combine =
            Validation.succeed Stay
                |> Validation.andMap name
                |> Validation.andMap
                    (Validation.succeed
                        (\checkinValue checkoutValue checkInTimeValue ->
                            Validation.succeed
                                { date = checkinValue
                                , nights = Date.toRataDie checkoutValue - Date.toRataDie checkinValue
                                , time = checkInTimeValue
                                }
                                |> Validation.withErrorIf (Date.toRataDie checkinValue >= Date.toRataDie checkoutValue) checkIn "Must be before checkout"
                        )
                        |> Validation.andMap checkIn
                        |> Validation.andMap checkOut
                        |> Validation.andMap checkInTime
                        |> Validation.andThen identity
                    )
                |> Validation.andMap emailUpdates
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
                [ fieldView "Name" name
                , fieldView "Check-In" checkIn
                , fieldView "Check-Out" checkOut
                , fieldView "Check-In Time" checkInTime
                , fieldView "Sign Up For Email Updates" emailUpdates
                , Html.button [] [ Html.text "Submit" ]
                ]
        }
    )
        |> Form.form
        |> Form.field "name"
            (Field.text
                |> Field.required "Required"
            )
        |> Form.field "checkin"
            (Field.date
                { invalid = \_ -> "Invalid" }
                |> Field.required "Required"
                |> Field.withMin today ("Must be after " ++ Date.toIsoString today)
            )
        |> Form.field "checkout"
            (Field.date
                { invalid = \_ -> "Invalid" }
                |> Field.required "Required"
            )
        |> Form.field "checkinTime"
            (Field.time
                { invalid = \_ -> "Invalid" }
                |> Field.required "Required"
                |> Field.withMin { hours = 10, minutes = 0, seconds = Nothing } "Must be after today"
            )
        |> Form.field "emailUpdates"
            Field.checkbox


today : Date
today =
    Date.fromRataDie 738624


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

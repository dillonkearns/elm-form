# `dillonkearns/elm-form` [![Build Status](https://github.com/dillonkearns/elm-form/workflows/CI/badge.svg)](https://github.com/dillonkearns/elm-form/actions?query=branch%3Amain)

Live Ellie demo: <https://ellie-app.com/mzjFg6BWmMta1>

`elm-form` is built around the idea
of managing a single [`Form.Model`](https://package.elm-lang.org/packages/dillonkearns/elm-form/3.0.1/Form/#Model) value as an unparsed set of raw field values and [`FieldStatus` (blurred, changed, etc.)](https://package.elm-lang.org/packages/dillonkearns/elm-form/3.0.1/Form-Validation/#FieldStatus).
This `Form.Model` can even handle form state of more than one form on a page, or even across multiple pages.
The package manages all of the unparsed state for you with a single `Msg`, a single `Model` entry, and then
uses your [`Form`](https://package.elm-lang.org/packages/dillonkearns/elm-form/3.0.1/Form/#Form) definition to run its validations against the unparsed values ([`Model`](https://package.elm-lang.org/packages/dillonkearns/elm-form/3.0.1/Form/#Model)),
and to render the form fields along with any validation errors.

If you use `elm-form` with `elm-pages`, the wiring is built into the framework so you don't need to wire in `update` or `Model`
yourself, and the framework manages additional Form state for you such as in-flight form submissions. The ideas in this
package originally came from `elm-pages`, but they are useful in a standalone context as well so this was split into
a separate package.

Some of these underlying ideas were discussed in [the Elm Radio episode Exploring a New Form API Design](https://elm-radio.com/episode/exploring-form-api).

## Core Values

- Progressive Enhancement - to make things more robust, and to leverage existing standards to get features for free instead of re-inventing them for every app. Forms are great for sending data to servers, lets use them! Let's go back to our Web fundamentals - it's worth reading [the MDN docs on form submissions](https://developer.mozilla.org/en-US/docs/Learn/Forms/Sending_and_retrieving_form_data) to understand the core building blocks.
- Accessibility - using standards to provide an experience that supports a broad range of users and use cases

## Core Ideas

- [`Form.Validation`](https://package.elm-lang.org/packages/dillonkearns/elm-form/3.0.1/Form-Validation/) lets you build up validations and parse fields into a combined value in the same pass (if you wanted to, you could even parse into a `Json.Encode.Value` or some payload to send to an API `onSubmit`)
- [`Form.Field`](https://package.elm-lang.org/packages/dillonkearns/elm-form/3.0.1/Form-Field/) lets you declare the fields (in the applicative pipeline in the Form definition)
- You can pass an input value when you render the form which can be used in rendering the view, and for getting initial values ([`withInitialValue`](https://package.elm-lang.org/packages/dillonkearns/elm-form/3.0.1/Form-Field/#withInitialValue))

## Opinions

- Forms are always rendered within a `<form>` element for accessibility, and to enable progressive enhancement
- Fields are always rendered within a form field element of some kind (<input> or <textarea>). Rendering the view for Fields with the appropriate attributes is done by the [`Form.FieldView` module](https://package.elm-lang.org/packages/dillonkearns/elm-form/3.0.1/Form-FieldView/). It can be rendered as [`elm/html`](https://package.elm-lang.org/packages/elm/html/latest/) or [`elm-css`](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/) elements (since those are the two basic ways to render semantic HTML field tags, `<input>` and `<textarea>`). `elm-ui` doesn't currently have a way to render semantic HTML tags for forms (`<form>` tag), so there aren't any `elm-ui` helpers at the moment, though you can always render to `elm/html`.

## Wiring

Many Elm form examples and APIs use the pattern of handling each changed field within
the `update` function. [For example, `elm-spa-example` uses this pattern in the Settings route](https://github.com/rtfeldman/elm-spa-example/blob/cb32acd73c3d346d0064e7923049867d8ce67193/src/Page/Settings.elm#L210-L214) (and throughout the app).

❗️🛑 NOTE: This code below is NOT the pattern this package is built on ❗️🛑

```elm
type alias Model =
    { username : String
    , avatar : String
    -- ... an entry for each form field here
    -- ... any additional app state
    }

update msg model =
    EnteredUsername username ->
        updateForm (\form -> { form | username = username }) model

    EnteredAvatar avatar ->
        updateForm (\form -> { form | avatar = avatar }) model
    -- .. additional handling for the remaining form fields

viewForm form =
    Html.form [ onSubmit (SubmittedForm form) ]
        [ input
            [ onInput EnteredAvatar
            , value form.avatar
            -- other attributes
            ]
            []
        -- , ... input elements for other form fields
        ]
```

This package tries to reduce boilerplate and manage form validations in a more declarative style
by parsing/validating the form as a whole rather than parsing/validating
each individual field. Here is [the same Settings route with `elm-pages` and `elm-form` for reference](https://github.com/dillonkearns/elm-pages-realworld/blob/main/app/Route/Settings.elm).

✅👇 NOTE: the code below is the wiring pattern we use in this package. ✅👇

Instead of wiring in different Msg's and Model fields for each individual form field, the wiring in this package is done once for all form state like this:

```elm
type Msg
    = FormMsg (Form.Msg Msg)
    | OnSubmit (Form.Validated String SignUpForm)
    -- | ... Other Msg's for your app

type alias Model =
    { formModel : Form.Model
    , submitting : Bool
    -- , ... additional state for your app
    }

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { formModel = Form.init
      , submitting = False
      }
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnSubmit parsed ->
            case parsed of
                Form.Valid signUpData ->
                    ( { model | submitting = True }
                    , sendSignUpData signUpData )
                Form.Invalid _ _ ->
                    -- validation errors are displayed already so
                    -- we don't need to do anything else here
                    ( model, Cmd.none )

        FormMsg formMsg ->
            let
                ( updatedFormModel, cmd ) =
                    Form.update formMsg model.formModel
            in
            ( { model | formModel = updatedFormModel }, cmd )


formView : Model -> Html Msg
formView model =
    signUpForm
        |> Form.renderHtml
        { submitting = model.submitting
        , state = model.formModel
        , toMsg = FormMsg
        }
        (Form.options "form"
            |> Form.withOnSubmit (\{parsed} -> OnSubmit parsed)
        )
        []

-- this is our parsed/validated type, but it can be anything we want,
-- including Json.Encode.Value, etc.
type alias SignUpForm =
    { username : String, password : String }


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
        |> Form.field "username" (Field.text |> Field.required "Required")
        |> Form.field "password" (Field.text |> Field.password |> Field.required "Required")
        |> Form.field "password-confirmation" (Field.text |> Field.password |> Field.required "Required")
```

This package is designed to be hooked into frameworks, whether it's a published framework like elm-pages (which has a built-in integration),
or your own internal framework. See the elm-pages docs for more details on how to render and submit your form using elm-pages.

module Form exposing
    ( Form, HtmlForm, StyledHtmlForm, DoneForm
    , form
    , field
    , Context
    , Errors, errorsForField
    , renderHtml, renderStyledHtml
    , Options, options
    , withInput, withAction, withOnSubmit, withServerResponse
    , withGetMethod
    , Method(..), methodToString
    , parse
    , hiddenField, hiddenKind
    , dynamic
    , Msg, Model, init, update
    , FormState, FieldState
    , Validated(..)
    , ServerResponse
    , mapMsg, toResult
    -- subGroup
    )

{-|


## Example

Let's look at a sign-up form example.

What to look for:

**The field declarations**

Below the `Form.form` call you will find all of the form's fields declared with

    |> Form.field ...

These are the form's field declarations.

These fields each have individual validations. For example, `|> Field.required ...` means we'll get a validation
error if that field is empty (similar for checking the minimum password length). This field definition defines
some information that will be used when render the field using [`Form.FieldView`](Form-FieldView) (whether it is a date input, password input, etc.).

There will be a corresponding parameter in the function we pass in to `Form.form` for every
field declaration (in this example, `\email password passwordConfirmation -> ...`).

**The `combine` validation**

In addition to the validation errors that individual fields can have independently (like
required fields or minimum password length), we can also do _dependent validations_.

We use the [`Form.Validation`](Form-Validation) module to take each individual field and combine
them into a type and/or errors.

**The `view`**

In your view you can lay out your fields however you want. While you will be using [`Form.FieldView`](Form-FieldView)
to render the fields themselves, the rendering for everything besides the fields (including `label`'s, `div`s, etc.) is completely up to you.

    import Form
    import Form.Field as Field
    import Form.FieldView as FieldView
    import Form.Validation as Validation
    import Html exposing (Html)
    import Html.Attributes as Attr
    import Route
    import Server.Request as Request
    import Server.Response exposing (Response)

    type alias SignupForm =
        { email : String
        , password : String
        }

    signupForm : Form.HtmlForm String SignupForm () Msg
    signupForm =
        Form.form
            (\email password passwordConfirmation ->
                { combine =
                    Validation.succeed SignupForm
                        |> Validation.andMap email
                        |> Validation.andMap
                            (Validation.map2
                                (\passwordValue confirmationValue ->
                                    if passwordValue == confirmationValue then
                                        Validation.succeed passwordValue

                                    else
                                        passwordConfirmation
                                            |> Validation.fail
                                                "Must match password"
                                )
                                password
                                passwordConfirmation
                                |> Validation.andThen identity
                            )
                , view =
                    \info ->
                        [ Html.label []
                            [ fieldView info "Email" email
                            , fieldView info "Password" password
                            , fieldView info "Confirm Password" passwordConfirmation
                            ]
                        , Html.button []
                            [ if info.submitting then
                                Html.text "Signing Up..."

                              else
                                Html.text "Sign Up"
                            ]
                        ]
                }
            )
            |> Form.field "email"
                (Field.text
                    |> Field.required "Required"
                )
            |> Form.field "password"
                passwordField
            |> Form.field "passwordConfirmation"
                passwordField

    passwordField =
        Field.text
            |> Field.password
            |> Field.required "Required"
            |> Field.withClientValidation
                (\password ->
                    ( Just password
                    , if String.length password < 4 then
                        [ "Must be at least 4 characters" ]

                      else
                        []
                    )
                )

    fieldView :
        Form.Context String input
        -> String
        -> Validation.Field String parsed FieldView.Input
        -> Html msg
    fieldView formState label field =
        Html.div []
            [ Html.label []
                [ Html.text (label ++ " ")
                , field |> Form.FieldView.input []
                ]
            , (if formState.submitAttempted then
                formState.errors
                    |> Form.errorsForField field
                    |> List.map
                        (\error ->
                            Html.li [] [ Html.text error ]
                        )

               else
                []
              )
                |> Html.ul [ Attr.style "color" "red" ]
            ]


## Building a Form Parser

@docs Form, HtmlForm, StyledHtmlForm, DoneForm

@docs form


### Adding Fields

@docs field


## View Functions

@docs Context


## Showing Errors

@docs Errors, errorsForField


## Rendering Forms

@docs renderHtml, renderStyledHtml

@docs Options, options

@docs withInput, withAction, withOnSubmit, withServerResponse

@docs withGetMethod

@docs Method, methodToString


## Running Parsers

@docs parse


## Progressively Enhanced Form Techniques (elm-pages)


### Hidden Fields

Hidden fields are a useful technique when you are progressively enhancing form submissions and sending the key-value form data directly.
In `elm-pages` apps this is used often and is an idiomatic approach. If you are wiring up your own `onSubmit` with a Msg
and never submit the forms directly, then you will likely include additional context as part of your `Msg` instead of
through hidden fields.

@docs hiddenField, hiddenKind


## Dynamic Fields

@docs dynamic


## Wiring

`elm-form` manages the client-side state of fields, including FieldStatus which you can use to determine when
in the user's workflow to show validation errors.

@docs Msg, Model, init, update

@docs FormState, FieldState

@docs Validated

@docs ServerResponse

@docs mapMsg, toResult

-}

import Dict exposing (Dict)
import Form.Field as Field exposing (Field)
import Form.FieldStatus
import Form.FieldView
import Form.Validation exposing (Combined)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Lazy
import Html.Styled
import Html.Styled.Attributes as StyledAttr
import Html.Styled.Lazy
import Internal.Field
import Internal.FieldEvent exposing (Event(..), FieldEvent)
import Internal.Form
import Internal.Input
import Pages.FormState as Form
import Pages.Internal.Form exposing (Validation(..))
import Task


{-| -}
type Validated error value
    = Valid value
    | Invalid (Maybe value) (Dict String (List error))


{-| -}
toResult : Validated error value -> Result ( Maybe value, Dict String (List error) ) value
toResult validated =
    case validated of
        Valid value ->
            Ok value

        Invalid maybeParsed errors ->
            Err ( maybeParsed, errors )


{-| -}
initFormState : FormState
initFormState =
    { fields = Dict.empty
    , submitAttempted = False
    }


{-| The data available as the first parameter in a Form's `view` function.
-}
type alias Context error input =
    { errors : Errors error
    , submitting : Bool
    , submitAttempted : Bool
    , input : input
    }


{-| Initialize the builder for a `Form`. Typically an anonymous function is passed in to this function, with one
parameter for each field that comes after.

    form =
        Form.form
            (\name email ->
                { combine =
                    Validation.succeed User
                        |> Validation.andMap name
                        |> Validation.andMap email
                , view = \info -> [{- render fields -}]
                }
            )
            |> Form.field "name" (Field.text |> Field.required "Required")
            |> Form.field "email" (Field.text |> Field.required "Required")

-}
form : combineAndView -> Form String combineAndView parsed input
form combineAndView =
    Internal.Form.Form
        []
        (\_ _ ->
            { result = Dict.empty
            , combineAndView = combineAndView
            , isMatchCandidate = True
            }
        )
        (\_ -> [])


{-| -}
dynamic :
    (decider
     ->
        Form
            error
            { combine : Form.Validation.Validation error parsed named constraints1
            , view : subView
            }
            parsed
            input
    )
    ->
        Form
            error
            --((decider -> Validation error parsed named) -> combined)
            ({ combine : decider -> Form.Validation.Validation error parsed named constraints1
             , view : decider -> subView
             }
             -> combineAndView
            )
            parsed
            input
    ->
        Form
            error
            combineAndView
            parsed
            input
dynamic forms formBuilder =
    Internal.Form.Form
        []
        (\maybeData formState ->
            let
                toParser :
                    decider
                    ->
                        { result : Dict String (List error)
                        , isMatchCandidate : Bool
                        , combineAndView : { combine : Validation error parsed named constraints1, view : subView }
                        }
                toParser decider =
                    case forms decider of
                        Internal.Form.Form _ parseFn _ ->
                            -- TODO need to include hidden form fields from `definitions` (should they be automatically rendered? Does that mean the view type needs to be hardcoded?)
                            parseFn maybeData formState

                myFn :
                    { result : Dict String (List error)
                    , isMatchCandidate : Bool
                    , combineAndView : combineAndView
                    }
                myFn =
                    let
                        newThing :
                            { result : Dict String (List error)
                            , isMatchCandidate : Bool
                            , combineAndView : { combine : decider -> Validation error parsed named constraints1, view : decider -> subView } -> combineAndView
                            }
                        newThing =
                            case formBuilder of
                                Internal.Form.Form _ parseFn _ ->
                                    parseFn maybeData formState

                        arg : { combine : decider -> Validation error parsed named constraints1, view : decider -> subView }
                        arg =
                            { combine =
                                toParser
                                    >> .combineAndView
                                    >> .combine
                            , view =
                                \decider ->
                                    decider
                                        |> toParser
                                        |> .combineAndView
                                        |> .view
                            }
                    in
                    { result =
                        newThing.result
                    , combineAndView =
                        newThing.combineAndView arg
                    , isMatchCandidate = newThing.isMatchCandidate
                    }
            in
            myFn
        )
        (\_ -> [])



--{-| -}
--subGroup :
--    Form error ( Maybe parsed, Dict String (List error) ) input (Context error input -> subView)
--    ->
--        Form
--            error
--            ({ value : parsed } -> combined)
--            input
--            (Context error input -> (subView -> combinedView))
--    -> Form error combined input (Context error input -> combinedView)
--subGroup forms formBuilder =
--    Form []
--        (\maybeData formState ->
--            let
--                toParser : { result : ( Maybe ( Maybe parsed, Dict String (List error) ), Dict String (List error) ), view : Context error input -> subView }
--                toParser =
--                    case forms of
--                        Form definitions parseFn toInitialValues ->
--                            -- TODO need to include hidden form fields from `definitions` (should they be automatically rendered? Does that mean the view type needs to be hardcoded?)
--                            parseFn maybeData formState
--
--                myFn :
--                    { result : ( Maybe combined, Dict String (List error) )
--                    , view : Context error input -> combinedView
--                    }
--                myFn =
--                    let
--                        deciderToParsed : ( Maybe parsed, Dict String (List error) )
--                        deciderToParsed =
--                            toParser |> mergeResults
--
--                        newThing : { result : ( Maybe ({ value : parsed } -> combined), Dict String (List error) ), view : Context error input -> subView -> combinedView }
--                        newThing =
--                            case formBuilder of
--                                Form definitions parseFn toInitialValues ->
--                                    parseFn maybeData formState
--
--                        anotherThing : Maybe combined
--                        anotherThing =
--                            Maybe.map2
--                                (\runFn parsed ->
--                                    runFn { value = parsed }
--                                )
--                                (Tuple.first newThing.result)
--                                (deciderToParsed |> Tuple.first)
--                    in
--                    { result =
--                        ( anotherThing
--                        , mergeErrors (newThing.result |> Tuple.second)
--                            (deciderToParsed |> Tuple.second)
--                        )
--                    , view =
--                        \fieldErrors ->
--                            let
--                                something2 : subView
--                                something2 =
--                                    fieldErrors
--                                        |> (toParser
--                                                |> .view
--                                           )
--                            in
--                            newThing.view fieldErrors something2
--                    }
--            in
--            myFn
--        )
--        (\_ -> [])


{-| Declare a visible field for the form.

Use [`Form.Field`](Form-Field) to define the field and its validations.

    form =
        Form.form
            (\email ->
                { combine =
                    Validation.succeed SignupForm
                        |> Validation.andMap email
                , view = \info -> [{- render fields -}]
                }
            )
            |> Form.field "email"
                (Field.text |> Field.required "Required")

-}
field :
    String
    -> Field error parsed input initial kind constraints
    -> Form error (Form.Validation.Field error parsed kind -> combineAndView) parsedCombined input
    -> Form error combineAndView parsedCombined input
field name (Internal.Field.Field fieldParser kind) (Internal.Form.Form definitions parseFn toInitialValues) =
    Internal.Form.Form
        (( name, Internal.Form.RegularField )
            :: definitions
        )
        (\maybeData formState ->
            let
                ( maybeParsed, errors ) =
                    -- @@@@@@ use code from here
                    fieldParser.decode rawFieldValue

                ( rawFieldValue, fieldStatus ) =
                    case formState.fields |> Dict.get name of
                        Just info ->
                            ( Just info.value, info.status )

                        Nothing ->
                            ( maybeData |> Maybe.andThen (\data -> fieldParser.initialValue data), Form.FieldStatus.notVisited )

                thing : Pages.Internal.Form.ViewField kind
                thing =
                    { value = rawFieldValue
                    , status = fieldStatus
                    , kind = ( kind, fieldParser.properties )
                    }

                parsedField : Form.Validation.Field error parsed kind
                parsedField =
                    Pages.Internal.Form.Validation (Just thing) (Just name) ( maybeParsed, Dict.empty )

                myFn :
                    { result : Dict String (List error)
                    , combineAndView : Form.Validation.Field error parsed kind -> combineAndView
                    , isMatchCandidate : Bool
                    }
                    ->
                        { result : Dict String (List error)
                        , combineAndView : combineAndView
                        , isMatchCandidate : Bool
                        }
                myFn soFar =
                    let
                        validationField : Form.Validation.Field error parsed kind
                        validationField =
                            parsedField
                    in
                    { result =
                        soFar.result
                            |> addErrorsInternal name errors
                    , combineAndView =
                        soFar.combineAndView validationField
                    , isMatchCandidate = soFar.isMatchCandidate
                    }
            in
            formState
                |> parseFn maybeData
                |> myFn
        )
        (\input ->
            case fieldParser.initialValue input of
                Just initialValue ->
                    ( name, Just initialValue )
                        :: toInitialValues input

                Nothing ->
                    toInitialValues input
        )


{-| Declare a hidden field for the form.

Unlike [`field`](#field) declarations which are rendered using [`Form.FieldView`](Form-FieldView)
functions, `hiddenField` inputs are automatically inserted into the form when you render it.

You define the field's validations the same way as for `field`, with the
[`Form.Field`](Form-Field) API.

    form =
        Form.form
            (\quantity productId ->
                { combine = {- combine fields -}
                , view = \info -> [{- render visible fields -}]
                }
            )
            |> Form.field "quantity"
                (Field.int |> Field.required "Required")
            |> Form.field "productId"
                (Field.text
                    |> Field.required "Required"
                    |> Field.withInitialValue (\product -> Form.Value.string product.id)
                )

-}
hiddenField :
    String
    -> Field error parsed input initial kind constraints
    -> Form error (Form.Validation.Field error parsed Form.FieldView.Hidden -> combineAndView) parsedCombined input
    -> Form error combineAndView parsedCombined input
hiddenField name (Internal.Field.Field fieldParser _) (Internal.Form.Form definitions parseFn toInitialValues) =
    Internal.Form.Form
        (( name, Internal.Form.HiddenField )
            :: definitions
        )
        (\maybeData formState ->
            let
                ( maybeParsed, errors ) =
                    fieldParser.decode rawFieldValue

                ( rawFieldValue, fieldStatus ) =
                    case formState.fields |> Dict.get name of
                        Just info ->
                            ( Just info.value, info.status )

                        Nothing ->
                            ( maybeData |> Maybe.andThen (\data -> fieldParser.initialValue data), Form.FieldStatus.notVisited )

                thing : Pages.Internal.Form.ViewField Form.FieldView.Hidden
                thing =
                    { value = rawFieldValue
                    , status = fieldStatus
                    , kind = ( Internal.Input.Hidden, fieldParser.properties )
                    }

                parsedField : Form.Validation.Field error parsed Form.FieldView.Hidden
                parsedField =
                    Pages.Internal.Form.Validation (Just thing) (Just name) ( maybeParsed, Dict.empty )

                myFn :
                    { result : Dict String (List error)
                    , combineAndView : Form.Validation.Field error parsed Form.FieldView.Hidden -> combineAndView
                    , isMatchCandidate : Bool
                    }
                    ->
                        { result : Dict String (List error)
                        , combineAndView : combineAndView
                        , isMatchCandidate : Bool
                        }
                myFn soFar =
                    let
                        validationField : Form.Validation.Field error parsed Form.FieldView.Hidden
                        validationField =
                            parsedField
                    in
                    { result =
                        soFar.result
                            |> addErrorsInternal name errors
                    , combineAndView =
                        soFar.combineAndView validationField
                    , isMatchCandidate = soFar.isMatchCandidate
                    }
            in
            formState
                |> parseFn maybeData
                |> myFn
        )
        (\input ->
            case fieldParser.initialValue input of
                Just initialValue ->
                    ( name, Just initialValue )
                        :: toInitialValues input

                Nothing ->
                    toInitialValues input
        )


{-| -}
hiddenKind :
    ( String, String )
    -> error
    -> Form error combineAndView parsed input
    -> Form error combineAndView parsed input
hiddenKind ( name, value ) error_ (Internal.Form.Form definitions parseFn toInitialValues) =
    let
        (Internal.Field.Field fieldParser _) =
            Field.exactValue value error_
    in
    Internal.Form.Form
        (( name, Internal.Form.HiddenField )
            :: definitions
        )
        (\maybeData formState ->
            let
                ( decodedValue, errors ) =
                    fieldParser.decode rawFieldValue

                rawFieldValue : Maybe String
                rawFieldValue =
                    case formState.fields |> Dict.get name of
                        Just info ->
                            Just info.value

                        Nothing ->
                            maybeData |> Maybe.andThen (\data -> fieldParser.initialValue data)

                myFn :
                    { result : Dict String (List error)
                    , isMatchCandidate : Bool
                    , combineAndView : combineAndView
                    }
                    ->
                        { result : Dict String (List error)
                        , isMatchCandidate : Bool
                        , combineAndView : combineAndView
                        }
                myFn soFar =
                    { result =
                        soFar.result
                            |> addErrorsInternal name errors
                    , combineAndView = soFar.combineAndView
                    , isMatchCandidate = soFar.isMatchCandidate && decodedValue == Just value
                    }
            in
            formState
                |> parseFn maybeData
                |> myFn
        )
        (\input ->
            ( name, Just value )
                :: toInitialValues input
        )


{-| -}
type Errors error
    = Errors (Dict String (List error))


{-| -}
errorsForField : Form.Validation.Field error parsed kind -> Errors error -> List error
errorsForField field_ (Errors errorsDict) =
    errorsDict
        |> Dict.get (Form.Validation.fieldName field_)
        |> Maybe.withDefault []


mergeResults :
    { a | result : ( Validation error parsed named constraints1, Dict String (List error) ) }
    -> Validation error parsed unnamed constraints2
mergeResults parsed =
    let
        ( Pages.Internal.Form.Validation _ name ( parsedThing, combineErrors ), individualFieldErrors ) =
            parsed.result
    in
    Pages.Internal.Form.Validation Nothing
        name
        ( parsedThing
        , mergeErrors combineErrors individualFieldErrors
        )


mergeErrors : Dict comparable (List value) -> Dict comparable (List value) -> Dict comparable (List value)
mergeErrors errors1 errors2 =
    Dict.merge
        (\key entries soFar ->
            soFar |> insertIfNonempty key entries
        )
        (\key entries1 entries2 soFar ->
            soFar |> insertIfNonempty key (entries1 ++ entries2)
        )
        (\key entries soFar ->
            soFar |> insertIfNonempty key entries
        )
        errors1
        errors2
        Dict.empty


{-| -}
parse :
    String
    -> Model
    -> input
    -> Form error { info | combine : Form.Validation.Validation error parsed named constraints } parsed input
    -> Validated error parsed
parse formId state input (Internal.Form.Form _ parser _) =
    -- TODO Get transition context from `app` so you can check if the current form is being submitted
    -- TODO either as a transition or a fetcher? Should be easy enough to check for the `id` on either of those?
    let
        parsed :
            { result : Dict String (List error)
            , isMatchCandidate : Bool
            , combineAndView : { info | combine : Validation error parsed named constraints }
            }
        parsed =
            parser (Just input) thisFormState

        thisFormState : Form.FormState
        thisFormState =
            state
                |> Dict.get formId
                |> Maybe.withDefault initFormState
                |> convert
    in
    case
        { result = ( parsed.combineAndView.combine, parsed.result )
        }
            |> mergeResults
            |> unwrapValidation
    of
        ( Just justParsed, errors ) ->
            if Dict.isEmpty errors then
                Valid justParsed

            else
                Invalid (Just justParsed) errors

        ( Nothing, errors ) ->
            Invalid Nothing errors


convert : FormState -> Form.FormState
convert formState =
    { submitAttempted = formState.submitAttempted
    , fields =
        formState.fields
            |> Dict.map
                (\_ value ->
                    { value = value.value
                    , status = statusRank value.status
                    }
                )
    }


insertIfNonempty : comparable -> List value -> Dict comparable (List value) -> Dict comparable (List value)
insertIfNonempty key values dict =
    if values |> List.isEmpty then
        dict

    else
        dict
            |> Dict.insert key values


unwrapValidation : Validation error parsed named constraints -> ( Maybe parsed, Dict String (List error) )
unwrapValidation (Pages.Internal.Form.Validation _ _ ( maybeParsed, errors )) =
    ( maybeParsed, errors )


{-| Render the form to `elm/html`.

    view model =
        signUpForm
            |> Form.renderHtml
                { submitting = model.submitting
                , state = model.formState
                , toMsg = FormMsg
                }
                (Form.options "signUpForm")
                []

Note: In `elm-pages`, you'll want to use the `Pages.Form.renderHtml` function instead.

-}
renderHtml :
    { submitting : Bool
    , state : Model
    , toMsg : Msg mappedMsg -> mappedMsg
    }
    -> Options error parsed input mappedMsg
    -> List (Html.Attribute mappedMsg)
    ->
        Form
            error
            { combine : Form.Validation.Validation error parsed named constraints
            , view : Context error input -> List (Html mappedMsg)
            }
            parsed
            input
    -> Html mappedMsg
renderHtml state options_ attrs form_ =
    Html.Lazy.lazy4 renderHelper state options_ attrs form_


{-| Render the form to [`rtfeldman/elm-css`](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/).

    view model =
        signUpForm
            |> Form.renderStyledHtml
                { submitting = model.submitting
                , state = model.formState
                , toMsg = FormMsg
                }
                (Form.options "signUpForm")
                []

Note: In `elm-pages`, you'll want to use the `Pages.Form.renderStyledHtml` function instead.

-}
renderStyledHtml :
    { submitting : Bool
    , state : Model
    , toMsg : Msg mappedMsg -> mappedMsg
    }
    -> Options error parsed input mappedMsg
    -> List (Html.Styled.Attribute mappedMsg)
    ->
        Form
            error
            { combine : Form.Validation.Validation error parsed field constraints
            , view : Context error input -> List (Html.Styled.Html mappedMsg)
            }
            parsed
            input
    -> Html.Styled.Html mappedMsg
renderStyledHtml state options_ attrs form_ =
    Html.Styled.Lazy.lazy4 renderStyledHelper state options_ attrs form_


{-| The `persisted` state will be ignored if the client already has a form state. It is useful for persisting state between page loads. For example, `elm-pages` server-rendered routes
use this `persisted` state in order to show client-side validations and preserve form field state when a submission is done with JavaScript disabled in the user's browser.

`serverSideErrors` will show on the client-side error state until the form is re-submitted. For example, if you need to check that a username is unique, you can do so by including
an error in `serverSideErrors` in the response back from the server. The client-side form will show the error until the user changes the username and re-submits the form, allowing the
server to re-validate that input.

-}
type alias ServerResponse error =
    { persisted :
        { fields : Maybe (List ( String, String ))
        , clientSideErrors : Maybe (Dict String (List error))
        }
    , serverSideErrors : Dict String (List error)
    }


renderHelper :
    { submitting : Bool
    , state : Model
    , toMsg : Msg mappedMsg -> mappedMsg
    }
    -> Options error parsed input mappedMsg
    -> List (Html.Attribute mappedMsg)
    ->
        Form
            error
            { combine : Form.Validation.Validation error parsed named constraints
            , view : Context error input -> List (Html mappedMsg)
            }
            parsed
            input
    -> Html mappedMsg
renderHelper formState options_ attrs ((Internal.Form.Form _ _ _) as form_) =
    -- TODO Get transition context from `app` so you can check if the current form is being submitted
    -- TODO either as a transition or a fetcher? Should be easy enough to check for the `id` on either of those?
    let
        { hiddenInputs, children, parsed, fields, errors } =
            helperValues options_ toHiddenInput formState form_

        toHiddenInput : List (Html.Attribute mappedMsg) -> Html mappedMsg
        toHiddenInput hiddenAttrs =
            Html.input hiddenAttrs []
    in
    Html.form
        ((Form.listeners options_.id
            |> List.map (Attr.map (Internal.FieldEvent.FormFieldEvent >> formState.toMsg))
         )
            ++ [ Attr.method (methodToString options_.method)
               , Attr.novalidate True
               ]
            ++ ([ options_.action |> Maybe.map Attr.action ] |> List.filterMap identity)
            ++ [ Internal.FieldEvent.formDataOnSubmit
                    |> Attr.map
                        (\formDataThing ->
                            let
                                maybeFormMsg : Maybe mappedMsg
                                maybeFormMsg =
                                    options_.onSubmit
                                        |> Maybe.map
                                            (\onSubmit ->
                                                onSubmit
                                                    { fields = formDataThing.fields |> Maybe.withDefault fields
                                                    , action = formDataThing.action
                                                    , method =
                                                        case formDataThing.method of
                                                            Internal.FieldEvent.Get ->
                                                                Get

                                                            Internal.FieldEvent.Post ->
                                                                Post
                                                    , parsed =
                                                        case parsed of
                                                            Just justParsed ->
                                                                if Dict.isEmpty errors then
                                                                    Valid justParsed

                                                                else
                                                                    Invalid (Just justParsed) errors

                                                            Nothing ->
                                                                Invalid Nothing errors
                                                    }
                                            )
                            in
                            Internal.FieldEvent.Submit formDataThing maybeFormMsg
                                |> formState.toMsg
                        )
               ]
            ++ attrs
        )
        (hiddenInputs ++ children)


renderStyledHelper :
    { submitting : Bool
    , state : Model
    , toMsg : Msg mappedMsg -> mappedMsg
    }
    -> Options error parsed input mappedMsg
    -> List (Html.Styled.Attribute mappedMsg)
    ->
        Form
            error
            { combine : Form.Validation.Validation error parsed field constraints
            , view : Context error input -> List (Html.Styled.Html mappedMsg)
            }
            parsed
            input
    -> Html.Styled.Html mappedMsg
renderStyledHelper formState options_ attrs ((Internal.Form.Form _ _ _) as form_) =
    -- TODO Get transition context from `app` so you can check if the current form is being submitted
    -- TODO either as a transition or a fetcher? Should be easy enough to check for the `id` on either of those?
    let
        { hiddenInputs, children, parsed, fields, errors } =
            helperValues options_ toHiddenInput formState form_

        toHiddenInput : List (Html.Attribute msg) -> Html.Styled.Html msg
        toHiddenInput hiddenAttrs =
            Html.Styled.input (hiddenAttrs |> List.map StyledAttr.fromUnstyled) []
    in
    Html.Styled.form
        ((Form.listeners options_.id
            |> List.map (Attr.map (Internal.FieldEvent.FormFieldEvent >> formState.toMsg))
            |> List.map StyledAttr.fromUnstyled
         )
            ++ [ StyledAttr.method (methodToString options_.method)
               , StyledAttr.novalidate True
               ]
            ++ ([ options_.action |> Maybe.map StyledAttr.action ] |> List.filterMap identity)
            ++ [ Internal.FieldEvent.formDataOnSubmit
                    |> Attr.map
                        (\formDataThing ->
                            let
                                maybeFormMsg : Maybe mappedMsg
                                maybeFormMsg =
                                    options_.onSubmit
                                        |> Maybe.map
                                            (\onSubmit ->
                                                onSubmit
                                                    { fields = formDataThing.fields |> Maybe.withDefault fields
                                                    , action = formDataThing.action
                                                    , method =
                                                        case formDataThing.method of
                                                            Internal.FieldEvent.Get ->
                                                                Get

                                                            Internal.FieldEvent.Post ->
                                                                Post
                                                    , parsed =
                                                        case parsed of
                                                            Just justParsed ->
                                                                if Dict.isEmpty errors then
                                                                    Valid justParsed

                                                                else
                                                                    Invalid (Just justParsed) errors

                                                            Nothing ->
                                                                Invalid Nothing errors
                                                    }
                                            )
                            in
                            Internal.FieldEvent.Submit formDataThing maybeFormMsg
                                |> formState.toMsg
                        )
                    |> StyledAttr.fromUnstyled
               ]
            ++ attrs
        )
        ((hiddenInputs ++ children) |> List.map (Html.Styled.map (Internal.FieldEvent.UserMsg >> formState.toMsg)))


helperValues :
    Options error parsed input mappedMsg
    -> (List (Html.Attribute mappedMsg) -> view)
    ->
        { submitting : Bool
        , state : Model
        , toMsg : Msg mappedMsg -> mappedMsg
        }
    ->
        Form
            error
            { combine : Form.Validation.Validation error parsed field constraints
            , view : Context error input -> List view
            }
            parsed
            input
    -> { hiddenInputs : List view, children : List view, isValid : Bool, parsed : Maybe parsed, fields : List ( String, String ), errors : Dict String (List error) }
helperValues options_ toHiddenInput formState (Internal.Form.Form fieldDefinitions parser toInitialValues) =
    let
        initialValues : Dict String FieldState
        initialValues =
            toInitialValues options_.input
                |> List.filterMap
                    (\( key, maybeValue ) ->
                        maybeValue
                            |> Maybe.map
                                (\value ->
                                    ( key, { value = value, status = Form.Validation.NotVisited } )
                                )
                    )
                |> Dict.fromList

        part2 : Dict String FieldState
        part2 =
            formState.state
                |> Dict.get options_.id
                |> Maybe.withDefault
                    (options_.serverResponse
                        |> Maybe.andThen (.persisted >> .fields)
                        |> Maybe.map
                            (\fields ->
                                { fields =
                                    fields
                                        |> List.map (Tuple.mapSecond (\value -> { value = value, status = Form.Validation.NotVisited }))
                                        |> Dict.fromList
                                , submitAttempted = True
                                }
                            )
                        |> Maybe.withDefault initFormState
                    )
                |> .fields

        fullFormState : Dict String FieldState
        fullFormState =
            initialValues
                |> Dict.union part2

        parsed :
            { result : ( Form.Validation.Validation error parsed field constraints, Dict String (List error) )
            , isMatchCandidate : Bool
            , view : Context error input -> List view
            }
        parsed =
            { isMatchCandidate = parsed1.isMatchCandidate
            , view = parsed1.combineAndView.view
            , result = ( parsed1.combineAndView.combine, parsed1.result )
            }

        parsed1 :
            { result : Dict String (List error)
            , isMatchCandidate : Bool
            , combineAndView : { combine : Form.Validation.Validation error parsed field constraints, view : Context error input -> List view }
            }
        parsed1 =
            parser (Just options_.input) (convert thisFormState)

        withoutServerErrors : Form.Validation.Validation error parsed named constraints
        withoutServerErrors =
            parsed |> mergeResults

        withServerErrors : Form.Validation.Validation error parsed named constraints
        withServerErrors =
            mergeResults
                { parsed
                    | result =
                        parsed.result
                            |> Tuple.mapSecond
                                (\errors1 ->
                                    mergeErrors errors1
                                        (options_.serverResponse
                                            |> Maybe.andThen (.persisted >> .clientSideErrors)
                                            |> Maybe.withDefault Dict.empty
                                        )
                                )
                }

        thisFormState : FormState
        thisFormState =
            formState.state
                |> Dict.get options_.id
                |> Maybe.withDefault
                    (options_.serverResponse
                        |> Maybe.andThen (.persisted >> .fields)
                        |> Maybe.map
                            (\fields ->
                                { fields =
                                    fields
                                        |> List.map
                                            (Tuple.mapSecond
                                                (\value ->
                                                    { value = value
                                                    , status = Form.Validation.NotVisited
                                                    }
                                                )
                                            )
                                        |> Dict.fromList
                                , submitAttempted = True
                                }
                            )
                        |> Maybe.withDefault initSingle
                    )
                |> (\state -> { state | fields = fullFormState })

        rawFields : List ( String, String )
        rawFields =
            fullFormState |> Dict.toList |> List.map (Tuple.mapSecond .value)

        context : Context error input
        context =
            { errors =
                withServerErrors
                    |> unwrapValidation
                    |> Tuple.second
                    |> Errors
            , submitting = formState.submitting
            , submitAttempted = thisFormState.submitAttempted
            , input = options_.input
            }

        children : List view
        children =
            parsed.view context

        hiddenInputs : List view
        hiddenInputs =
            fieldDefinitions
                |> List.filterMap
                    (\( name, fieldDefinition ) ->
                        case fieldDefinition of
                            Internal.Form.HiddenField ->
                                [ Attr.name name
                                , Attr.type_ "hidden"
                                , Attr.value
                                    (initialValues
                                        |> Dict.get name
                                        |> Maybe.map .value
                                        |> Maybe.withDefault ""
                                    )
                                ]
                                    |> toHiddenInput
                                    |> Just

                            Internal.Form.RegularField ->
                                Nothing
                    )

        isValid : Bool
        isValid =
            case withoutServerErrors of
                Validation _ _ ( Just _, errors ) ->
                    Dict.isEmpty errors

                _ ->
                    False

        ( maybeParsed, errorsDict ) =
            case withoutServerErrors of
                Validation _ _ ( parsedValue, errors ) ->
                    ( parsedValue, errors )
    in
    { hiddenInputs = hiddenInputs
    , children = children
    , isValid = isValid
    , parsed = maybeParsed
    , fields = rawFields
    , errors = errorsDict
    }


initSingle : FormState
initSingle =
    { fields = Dict.empty
    , submitAttempted = False
    }


{-| -}
type alias DoneForm error parsed input view =
    Form
        error
        { combine : Combined error parsed
        , view : Context error input -> view
        }
        parsed
        input


{-| -}
type alias HtmlForm error parsed input msg =
    DoneForm error parsed input (List (Html msg))


{-| -}
type alias StyledHtmlForm error parsed input msg =
    DoneForm error parsed input (List (Html.Styled.Html msg))


{-| A `Form` definition represents

  - The fields of the form
  - How to render a form's fields into a `view`, and
  - How to `combine` the fields into a parsed value

A `Form` can be used to:


### Render

  - Render a `<form>` tag (using [`renderHtml`](#renderHtml) or [`renderStyledHtml`](#renderStyledHtml))


### Parse

  - [`parse`](#parse) into a [`Validated`](#Validated) value
  - You can attempt to parse one of multiple `Form` definitions using [`Form.Handler`](Form-Handler).

While you almost always will want to render your `Form` in your `view` function, you may also want to parse your form in a few more advanced use cases.

In a full-stack Elm application, you can try parsing with your Form definition since you can use code sharing to share the same `Form` definition between your frontend and backend.
`elm-pages` has several built-in functions to help with this.

You may also want to parse your form data in your frontend to take in-flight form submissions and parse them into your parsed values.

-}
type alias Form error combineAndView parsed input =
    Internal.Form.Form error combineAndView parsed input


{-| -}
addErrorsInternal : String -> List error -> Dict String (List error) -> Dict String (List error)
addErrorsInternal name newErrors allErrors =
    allErrors
        |> Dict.update name
            (\errors ->
                Just (newErrors ++ (errors |> Maybe.withDefault []))
            )


{-| -}
type alias Msg msg =
    Internal.FieldEvent.Msg msg


{-| -}
mapMsg : (msg -> msgMapped) -> Msg msg -> Msg msgMapped
mapMsg mapFn msg =
    case msg of
        Internal.FieldEvent.UserMsg userMsg ->
            Internal.FieldEvent.UserMsg (mapFn userMsg)

        Internal.FieldEvent.FormFieldEvent fieldEvent ->
            Internal.FieldEvent.FormFieldEvent fieldEvent

        Internal.FieldEvent.Submit formData maybeMsg ->
            Internal.FieldEvent.Submit formData (maybeMsg |> Maybe.map mapFn)


{-| The state for all forms. This is a single value that can be used to manage your form state, so when you render your
`Form`s you will get client-side validations based on the state managed through this value. The state that is
included here for each Form is:

  - Whether submit has been attempted on the form
  - The current value of each field in the form
  - The current [`Form.Validation.FieldStatus`](Form.Validation#FieldStatus) for each field in the form

Since this manages the state of multiple Forms, you can even maintain this in your application-wide `Model` rather than
in a page-specific `Model`. In an `elm-pages` application, this is managed through the framework, but you can achieve
a similar wiring by managing the `Form.Model` globally.

In more advanced cases, you can manually modify the state of a form. However, it's often enough to just let this package
manage the state for you through the [`Form.update`](Form#update) function. Since this is a `Dict String FormState`, you can use `Dict` operations to clear or update
the state of forms if you need to manually manage form state.

-}
type alias Model =
    Dict String FormState


{-| Initialize the [`Form.Model`](Form#Model).

    import Form

    init : Flags -> ( Model, Cmd Msg )
    init flags =
        ( { formModel = Form.init
          , submitting = False
          }
        , Cmd.none
        )

-}
init : Model
init =
    Dict.empty


{-| -}
update : Msg msg -> Model -> ( Model, Cmd msg )
update formMsg formModel =
    case formMsg of
        Internal.FieldEvent.UserMsg myMsg ->
            ( formModel
            , Task.succeed myMsg |> Task.perform identity
            )

        Internal.FieldEvent.FormFieldEvent value ->
            ( updateInternal value formModel
            , Cmd.none
            )

        Internal.FieldEvent.Submit formData maybeMsg ->
            ( setSubmitAttempted
                (formData.id |> Maybe.withDefault "form")
                formModel
            , maybeMsg
                |> Maybe.map (\userMsg -> Task.succeed userMsg |> Task.perform identity)
                |> Maybe.withDefault Cmd.none
            )


{-| -}
updateInternal : FieldEvent -> Model -> Model
updateInternal fieldEvent pageFormState =
    --if Dict.isEmpty pageFormState then
    --    -- TODO get all initial field values
    --    pageFormState
    --
    --else
    pageFormState
        |> Dict.update fieldEvent.formId
            (\previousValue_ ->
                let
                    previousValue : FormState
                    previousValue =
                        previousValue_
                            |> Maybe.withDefault initSingle
                in
                previousValue
                    |> updateForm fieldEvent
                    |> Just
            )


{-| -}
updateForm : FieldEvent -> FormState -> FormState
updateForm fieldEvent formState =
    { formState
        | fields =
            formState.fields
                |> Dict.update fieldEvent.name
                    (\previousValue_ ->
                        let
                            previousValue : FieldState
                            previousValue =
                                previousValue_
                                    |> Maybe.withDefault { value = fieldEvent.value, status = Form.Validation.NotVisited }
                        in
                        (case fieldEvent.event of
                            InputEvent newValue ->
                                { previousValue
                                    | value = newValue
                                    , status = previousValue.status |> increaseStatusTo Form.Validation.Changed
                                }

                            FocusEvent ->
                                { previousValue
                                    | status =
                                        previousValue.status
                                            |> increaseStatusTo Form.Validation.Focused
                                }

                            BlurEvent ->
                                { previousValue | status = previousValue.status |> increaseStatusTo Form.Validation.Blurred }
                        )
                            |> Just
                    )
    }


setSubmitAttempted : String -> Model -> Model
setSubmitAttempted fieldId pageFormState =
    pageFormState
        |> Dict.update fieldId
            (\maybeForm ->
                case maybeForm of
                    Just formState ->
                        Just { formState | submitAttempted = True }

                    Nothing ->
                        Just { initSingle | submitAttempted = True }
            )


{-| -}
increaseStatusTo : Form.Validation.FieldStatus -> Form.Validation.FieldStatus -> Form.Validation.FieldStatus
increaseStatusTo increaseTo currentStatus =
    if statusRank increaseTo > statusRank currentStatus then
        increaseTo

    else
        currentStatus


{-| -}
statusRank : Form.Validation.FieldStatus -> Int
statusRank status =
    case status of
        Form.Validation.NotVisited ->
            0

        Form.Validation.Focused ->
            1

        Form.Validation.Changed ->
            2

        Form.Validation.Blurred ->
            3


{-| -}
type alias Options error parsed input msg =
    -- TODO have a way to override path?
    -- TODO move method from Form options to here
    --path : Path
    { id : String
    , action : Maybe String
    , method : Method
    , input : input
    , onSubmit :
        Maybe
            ({ fields : List ( String, String ), method : Method, action : String, parsed : Validated error parsed }
             -> msg
            )
    , serverResponse : Maybe (ServerResponse error)
    }


{-| Initialize a set of default options with a unique `id` for your Form. Note that even if you are rendering the same form
multiple times this ID needs to be unique in order to manage the state of each form independently.

For example,

    cartView model items =
        items
            |> List.map
                (\item ->
                    itemForm
                        |> Form.renderHtml
                            { submitting = model.submitting
                            , state = model.formState
                            , toMsg = FormMsg
                            }
                            (Form.options ("cart-item-" ++ item.id))
                            []
                )
            |> Html.div []

-}
options : String -> Options error parsed () msg
options id =
    { id = id
    , action = Nothing
    , method = Post
    , input = ()
    , onSubmit = Nothing
    , serverResponse = Nothing
    }


{-| You can render your `Form` with an initial set of values and errors that semantically represent a server response.

Conceptually, this is like sending a traditional form submission to a backend. When this happens in a `<form>` with no
JavaScript event handlers, the server responds with a new page load, and that newly rendered page needs to contain any
field errors and persist any field values that were submitted so the user can continue filling out their form.

In an `elm-pages` app, you can submit your forms with JavaScript turned off and see this exact behavior, but you need to
be sure to wire in a `ServerResponse` so that the form state is persisted in the freshly rendered page.

You can also use this `ServerResponse` to send down server-side errors, especially if you are using full-stack Elm.

-}
withServerResponse : Maybe (ServerResponse error) -> Options error parsed input msg -> Options error parsed input msg
withServerResponse serverResponse options_ =
    { options_ | serverResponse = serverResponse }


{-| You can pass in an `input` value to the `Options` that are passed in to [`renderHtml`](#renderHtml) or [`renderStyledHtml`](#renderStyledHtml).

You can use whichever data type you want as your `input` value. You will then have access to that value in two places:

1.  The Form's `view` through the [`Context`](Context) argument's `input` field.
2.  [`Form.Field.withInitialValue`](Form-Field#withInitialValue)

One example where you would use an `input` value is if you have an existing UserProfile from the server that you want to use to pre-populate the form fields.

    import Form
    import Form.Field as Field
    import Form.Validation as Validation

    type alias UserProfile =
        { name : String
        , email : String
        }

    userProfileForm : Form.HtmlForm String UserProfile UserProfile msg
    userProfileForm =
        (\name email ->
            { combine =
                Validation.succeed UserProfile
                    |> Validation.andMap name
                    |> Validation.andMap email
            , view =
                \context ->
                    [ Html.h2
                        []
                        [ Html.text
                            --  use the input to display Model data
                            (context.input
                                ++ "'s Profile"
                            )
                        ]
                    , fieldView "Name" name
                    , fieldView "Email" email
                    , if context.submitting then
                        Html.button [ Html.Attributes.disabled True ] [ Html.text "Updating..." ]

                      else
                        Html.button [] [ Html.text "Update" ]
                    ]
            }
        )
            |> Form.form
            |> Form.field "name"
                (Field.text
                    |> Field.required "Required"
                    |> Field.withInitialValue .name
                )
            |> Form.field "email"
                (Field.text
                    |> Field.required "Required"
                    |> Field.withInitialValue .email
                )

    view model =
        [ model.userProfile
            |> Maybe.map
                (\userProfile ->
                    userProfileForm
                        |> Form.renderHtml
                            { submitting = model.submitting
                            , state = model.formState
                            , toMsg = FormMsg
                            }
                            (Form.options "userProfile"
                                |> Form.withInput userProfile
                            )
                            []
                )
            |> Maybe.withDefault "Loading Profile..."
        ]

-}
withInput : input -> Options error parsed () msg -> Options error parsed input msg
withInput input options_ =
    { id = options_.id
    , action = options_.action
    , input = input
    , onSubmit = options_.onSubmit
    , serverResponse = options_.serverResponse
    , method = options_.method
    }


{-| You can add an onSubmit handler to the Form's `Options`. If you are using a framework that is integrated with `elm-form` (such as `elm-pages`), then you can
rely on the framework's onSubmit behavior. Otherwise, you will need to do something with the form when there is a valid form submission.

There are a few different approaches you can use.

1.  Progressively enhance the raw FormData submission. Since `withOnSubmit` gives you access to `{ fields : List ( String, String ) {- ... -} }`, you can propagate the raw key-value pairs (`fields`) and send those to your API. If you are doing full-stack Elm with `elm-pages` or Lamdera, then this can be a great fit because you can do code sharing and re-use your `Form` definition on the backend to parse the raw FormData. However, you may not want to use this approach with frontend-only Elm because you may prefer to communicate with your backend using more structured data like JSON rather than FormData (which is just key-value strings).
2.  Parse into your preferred type, then with an on-submit Msg, check if the data is `Valid`, and if it is, use the parsed data to make a request to your API (by JSON-encoding the value, building a GraphQL request, etc.).
3.  In your Form's `combine`, directly parse into a representation of your request, such as a `Json.Encode.Value`, a `Cmd Msg`, a `Task error Msg`, or an intermediary data type that represents an API request.

Let's look at an example of approach (3). In this example, we define a `Request` record alias which represents an API request. Note, there is nothing special about this `Request` type, this is just
an example ot illustrate this general pattern, but consider the best types for your use case when you adapt this example for your app.

    import Form
    import Form.Field as Field
    import Form.Validation as Validation

    type alias Request =
        { path : String
        , body : Encode.Value
        , expect : Http.Expect Msg
        }

    sendRequest : Request -> Cmd Msg
    sendRequest request =
        Http.post
            { url = "https://myservice.com/api" ++ request.path
            , body = Http.jsonBody request.body
            , expect = request.expect
            }

    userProfileForm : Form.HtmlForm String Request input msg
    userProfileForm =
        (\name email ->
            { combine =
                Validation.succeed
                    (\nameValue emailValue ->
                        { path = "/api/user"
                        , body =
                            Encode.object
                                [ ( "name", Encode.string nameValue )
                                , ( "email", Encode.string emailValue )
                                ]
                        }
                    , expect = Http.expectJson GotUpdatedProfile profileDecoder
                    )
                    |> Validation.andMap name
                    |> Validation.andMap email
            , view = \context -> [{- ... view here -}]
            }
        )
            |> Form.form
            |> Form.field "name" (Field.text |> Field.required "Required")
            |> Form.field "email" (Field.text |> Field.required "Required")

-}
withOnSubmit : ({ fields : List ( String, String ), method : Method, action : String, parsed : Validated error parsed } -> msg) -> Options error parsed input oldMsg -> Options error parsed input msg
withOnSubmit onSubmit options_ =
    { id = options_.id
    , action = options_.action
    , input = options_.input
    , onSubmit = Just onSubmit
    , serverResponse = options_.serverResponse
    , method = options_.method
    }


{-| Set the `action` attribute of the rendered `<form>` element. Note that the `action` attribute in the `withOnSubmit` is preprocessed in the browser, so the String will point to the same URL but
won't necessarily be the exact same String that was passed in. For example, if you set `options |> Form.withAction "/login"`, your onSubmit will receive an absolute URL such as `{ action = "https://mysite.com/login" {- , ... -} }`.

Setting the `action` can be useful if you are progressively enhancing form behavior. The default browser form submission behavior is to submit to the current URL if no `action` attribute is set, and an `action` is present
then the form submission will go to the given URL. If you are attempting to use progressive enhancement then you can simulate this behavior through your `withOnSubmit` handler, or you may be using a framework that has this simulation built in like `elm-pages`.

See also <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form#action>.

-}
withAction : String -> Options error parsed input msg -> Options error parsed input msg
withAction action options_ =
    { options_ | action = Just action }


{-| -}
type Method
    = Get
    | Post


{-| The default Method from `options` is `Post` since that is the most common. The `Get` Method for form submissions will add the form fields as a query string and navigate to that route using a GET.
You will need to progressively enhance your onSubmit to simulate this browser behavior if you want something similar, or use a framework that has this simulation built in like `elm-pages`.
-}
withGetMethod : Options error parsed input msg -> Options error parsed input msg
withGetMethod options_ =
    { options_ | method = Get }


{-| -}
methodToString : Method -> String
methodToString method =
    case method of
        Get ->
            "GET"

        Post ->
            "POST"


{-| -}
type alias FieldState =
    { value : String
    , status : Form.Validation.FieldStatus
    }


{-| -}
type alias FormState =
    { fields : Dict String FieldState
    , submitAttempted : Bool
    }

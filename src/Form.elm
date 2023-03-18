module Form exposing
    ( Form, HtmlForm, StyledHtmlForm, DoneForm
    , Response
    , init
    , field, hiddenField, hiddenKind
    , Context
    , renderHtml, renderStyledHtml
    , withGetMethod
    , Errors, errorsForField
    , parse, runServerSide
    , dynamic
    , AppContext
    , withOnSubmit
    -- subGroup
    )

{-| One of the core features of elm-pages is helping you manage form data end-to-end, including

  - Presenting the HTML form with its fields
  - Maintaining client-side form state
  - Showing validation errors on the client-side
  - Receiving a form submission on the server-side
  - Using the exact same client-side validations on the server-side
  - Letting you run server-only Validations with BackendTask's (things like checking for a unique username)

Because elm-pages is a framework, it has its own internal Model and Msg's. That means you, the user,
can offload some of the responsibility to elm-pages and build an interactive form with real-time
client-side state and validation errors without wiring up your own Model and Msg's to manage that
state. You define the source of truth for your form (how to parse it into data or errors), and
elm-pages manages the state.

Let's look at a sign-up form example.


### Step 1 - Define the Form

What to look for:

**The field declarations**

Below the `Form.init` call you will find all of the form's fields declared with

    |> Form.field ...

These are the form's field declarations.

These fields each have individual validations. For example, `|> Field.required ...` means we'll get a validation
error if that field is empty (similar for checking the minimum password length).

There will be a corresponding parameter in the function we pass in to `Form.init` for every
field declaration (in this example, `\email password passwordConfirmation -> ...`).

**The `combine` validation**

In addition to the validation errors that individual fields can have independently (like
required fields or minimum password length), we can also do _dependent validations_.

We use the [`Form.Validation`](Form-Validation) module to take each individual field and combine
them into a type and/or errors.

**The `view`**

Totally customizable. Uses [`Form.FieldView`](Form-FieldView) to render all of the fields declared.

    import BackendTask exposing (BackendTask)
    import ErrorPage exposing (ErrorPage)
    import Form
    import Form.Field as Field
    import Form.FieldView as FieldView
    import Form.Validation as Validation
    import Html exposing (Html)
    import Html.Attributes as Attr
    import Route
    import Server.Request as Request
    import Server.Response exposing (Response)

    type alias NewUser =
        { email : String
        , password : String
        }

    signupForm : Form.HtmlForm String NewUser () Msg
    signupForm =
        Form.init
            (\email password passwordConfirmation ->
                { combine =
                    Validation.succeed Login
                        |> Validation.andMap email
                        |> Validation.andMap
                            (Validation.map2
                                (\pass confirmation ->
                                    if pass == confirmation then
                                        Validation.succeed pass

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
                            [ if info.isTransitioning then
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


### Step 2 - Render the Form's View

    view maybeUrl sharedModel app =
        { title = "Sign Up"
        , body =
            [ form
                |> Form.renderHtml "login" [] Nothing app ()
            ]
        }


### Step 3 - Handle Server-Side Form Submissions

    action : RouteParams -> Request.Parser (BackendTask (Response ActionData ErrorPage))
    action routeParams =
        Request.formData [ signupForm ]
            |> Request.map
                (\signupResult ->
                    case signupResult of
                        Ok newUser ->
                            newUser
                                |> myCreateUserBackendTask
                                |> BackendTask.map
                                    (\() ->
                                        -- redirect to the home page
                                        -- after successful sign-up
                                        Route.redirectTo Route.Index
                                    )

                        Err _ ->
                            Route.redirectTo Route.Login
                                |> BackendTask.succeed
                )

    myCreateUserBackendTask : BackendTask ()
    myCreateUserBackendTask =
        BackendTask.fail
            "TODO - make a database call to create a new user"


## Building a Form Parser

@docs Form, HtmlForm, StyledHtmlForm, DoneForm

@docs Response

@docs init


## Adding Fields

@docs field, hiddenField, hiddenKind


## View Functions

@docs Context


## Rendering Forms

@docs renderHtml, renderStyledHtml

@docs withGetMethod


## Showing Errors

@docs Errors, errorsForField


## Running Parsers

@docs parse, runServerSide


## Dynamic Fields

@docs dynamic

@docs AppContext


## Submission

@docs withOnSubmit

-}

import Dict exposing (Dict)
import Form.Field as Field exposing (Field(..))
import Form.FieldView
import Form.State exposing (FieldStatus, Msg)
import Form.Validation exposing (Combined)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Html.Lazy
import Html.Styled
import Html.Styled.Attributes as StyledAttr
import Html.Styled.Lazy
import Internal.Form
import Pages.FormState as Form exposing (FormState)
import Pages.Internal.Form exposing (Validation(..), unwrapResponse)


{-| -}
initFormState : FormState
initFormState =
    { fields = Dict.empty
    , submitAttempted = False
    }


{-| -}
type alias Context error input =
    { errors : Errors error
    , isTransitioning : Bool
    , submitAttempted : Bool
    , input : input
    }


{-| -}
init : combineAndView -> Form String combineAndView parsed input msg
init combineAndView =
    Internal.Form.Form
        { method = Internal.Form.Post
        , onSubmit = Nothing
        }
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
            msg
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
            msg
    ->
        Form
            error
            combineAndView
            parsed
            input
            msg
dynamic forms formBuilder =
    Internal.Form.Form
        { method = Internal.Form.Post
        , onSubmit = Nothing
        }
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
                        Internal.Form.Form _ _ parseFn _ ->
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
                                Internal.Form.Form _ _ parseFn _ ->
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
        Form.init
            (\email ->
                { combine =
                    Validation.succeed NewUser
                        |> Validation.andMap email
                , view = \info -> [{- render fields -}]
                }
            )
            |> Form.field "email"
                (Field.text |> Field.required "Required")

-}
field :
    String
    -> Field error parsed input kind constraints
    -> Form error (Form.Validation.Field error parsed kind -> combineAndView) parsedCombined input msg
    -> Form error combineAndView parsedCombined input msg
field name (Field fieldParser kind) (Internal.Form.Form renderOptions definitions parseFn toInitialValues) =
    Internal.Form.Form renderOptions
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
                            ( Maybe.map2 (|>) maybeData fieldParser.initialValue |> Maybe.andThen identity, Form.State.NotVisited )

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
            case fieldParser.initialValue of
                Just toInitialValue ->
                    ( name, toInitialValue input )
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
        Form.init
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
    -> Field error parsed input kind constraints
    -> Form error (Form.Validation.Field error parsed Form.FieldView.Hidden -> combineAndView) parsed input msg
    -> Form error combineAndView parsed input msg
hiddenField name (Field fieldParser _) (Internal.Form.Form options definitions parseFn toInitialValues) =
    Internal.Form.Form options
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
                            ( Maybe.map2 (|>) maybeData fieldParser.initialValue |> Maybe.andThen identity, Form.State.NotVisited )

                thing : Pages.Internal.Form.ViewField Form.FieldView.Hidden
                thing =
                    { value = rawFieldValue
                    , status = fieldStatus
                    , kind = ( Form.FieldView.Hidden, fieldParser.properties )
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
            case fieldParser.initialValue of
                Just toInitialValue ->
                    ( name, toInitialValue input )
                        :: toInitialValues input

                Nothing ->
                    toInitialValues input
        )


{-| -}
hiddenKind :
    ( String, String )
    -> error
    -> Form error combineAndView parsed input msg
    -> Form error combineAndView parsed input msg
hiddenKind ( name, value ) error_ (Internal.Form.Form options definitions parseFn toInitialValues) =
    let
        (Field fieldParser _) =
            Field.exactValue value error_
    in
    Internal.Form.Form options
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
                            Maybe.map2 (|>) maybeData fieldParser.initialValue
                                |> Maybe.andThen identity

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
            case fieldParser.initialValue of
                Just toInitialValue ->
                    ( name, toInitialValue input )
                        :: toInitialValues input

                Nothing ->
                    toInitialValues input
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


{-| -}
type alias AppContext =
    { --, sharedData : Shared.Data
      --, routeParams : routeParams
      path : List String

    --, action : Maybe actionData
    --, submit :
    --    { fields : List ( String, String ), headers : List ( String, String ) }
    --    -> Pages.Fetcher.Fetcher (Result Http.Error action)
    --, transition : Maybe Transition
    --, fetchers : Dict String (Pages.Transition.FetcherState (Maybe actionData))
    , isTransitioning : Bool
    , pageFormState :
        Dict String { fields : Dict String { value : String, status : FieldStatus }, submitAttempted : Bool }
    }


mergeResults :
    { a | result : ( Validation error parsed named constraints1, Dict String (List error) ) }
    -> Validation error parsed unnamed constraints2
mergeResults parsed =
    case parsed.result of
        ( Pages.Internal.Form.Validation _ name ( parsedThing, combineErrors ), individualFieldErrors ) ->
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
    -> AppContext
    -> input
    -> Form error { info | combine : Form.Validation.Validation error parsed named constraints } parsed input msg
    -> ( Maybe parsed, Dict String (List error) )
parse formId app input (Internal.Form.Form _ _ parser _) =
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

        thisFormState : FormState
        thisFormState =
            app.pageFormState
                |> Dict.get formId
                |> Maybe.withDefault initFormState
    in
    { result = ( parsed.combineAndView.combine, parsed.result )
    }
        |> mergeResults
        |> unwrapValidation


insertIfNonempty : comparable -> List value -> Dict comparable (List value) -> Dict comparable (List value)
insertIfNonempty key values dict =
    if values |> List.isEmpty then
        dict

    else
        dict
            |> Dict.insert key values


{-| -}
runServerSide :
    List ( String, String )
    -> Form error (Form.Validation.Validation error parsed kind constraints) Never input msg
    -> ( Bool, ( Maybe parsed, Dict String (List error) ) )
runServerSide rawFormData (Internal.Form.Form _ _ parser _) =
    let
        parsed :
            { result : Dict String (List error)
            , isMatchCandidate : Bool
            , combineAndView : Validation error parsed kind constraints
            }
        parsed =
            parser Nothing thisFormState

        thisFormState : FormState
        thisFormState =
            { initFormState
                | fields =
                    rawFormData
                        |> List.map
                            (Tuple.mapSecond
                                (\value ->
                                    { value = value
                                    , status = Form.State.NotVisited
                                    }
                                )
                            )
                        |> Dict.fromList
            }
    in
    ( parsed.isMatchCandidate
    , { result = ( parsed.combineAndView, parsed.result )
      }
        |> mergeResults
        |> unwrapValidation
    )


unwrapValidation : Validation error parsed named constraints -> ( Maybe parsed, Dict String (List error) )
unwrapValidation (Pages.Internal.Form.Validation _ _ ( maybeParsed, errors )) =
    ( maybeParsed, errors )


{-| -}
renderHtml :
    String
    -> List (Html.Attribute (Msg msg))
    -> (actionData -> Maybe (Response error))
    -> AppContext
    -> input
    ->
        Form
            error
            { combine : Form.Validation.Validation error parsed named constraints
            , view : Context error input -> List (Html (Msg msg))
            }
            parsed
            input
            msg
    -> Html (Msg msg)
renderHtml formId attrs accessResponse app input form =
    Html.Lazy.lazy6 renderHelper formId attrs accessResponse app input form


{-| -}
withGetMethod : Form error combineAndView parsed input userMsg -> Form error combineAndView parsed input userMsg
withGetMethod (Internal.Form.Form options a b c) =
    Internal.Form.Form { options | method = Internal.Form.Get } a b c


{-| -}
withOnSubmit : ({ fields : List ( String, String ), parsed : Result () parsed } -> userMsg) -> Form error combineAndView parsed input oldMsg -> Form error combineAndView parsed input userMsg
withOnSubmit onSubmit (Internal.Form.Form options a b c) =
    Internal.Form.Form
        { onSubmit = Just onSubmit
        , method = options.method
        }
        a
        b
        c


{-| -}
renderStyledHtml :
    String
    -> List (Html.Styled.Attribute (Msg msg))
    -> (actionData -> Maybe (Response error))
    -> AppContext
    -> input
    ->
        Form
            error
            { combine : Form.Validation.Validation error parsed field constraints
            , view : Context error input -> List (Html.Styled.Html (Msg msg))
            }
            parsed
            input
            msg
    -> Html.Styled.Html (Msg msg)
renderStyledHtml formId attrs accessResponse app input form =
    Html.Styled.Lazy.lazy6 renderStyledHelper formId attrs accessResponse app input form


{-| -}
type alias Response error =
    Pages.Internal.Form.Response error


renderHelper :
    String
    -> List (Html.Attribute (Msg msg))
    -> (actionData -> Maybe (Response error))
    -> AppContext
    -> input
    ->
        Form
            error
            { combine : Form.Validation.Validation error parsed named constraints
            , view : Context error input -> List (Html (Msg msg))
            }
            parsed
            input
            msg
    -> Html (Msg msg)
renderHelper formId attrs accessResponse formState input ((Internal.Form.Form options _ _ _) as form) =
    -- TODO Get transition context from `app` so you can check if the current form is being submitted
    -- TODO either as a transition or a fetcher? Should be easy enough to check for the `id` on either of those?
    let
        { hiddenInputs, children, isValid, parsed } =
            helperValues formId toHiddenInput accessResponse formState input form

        toHiddenInput : List (Html.Attribute (Msg msg)) -> Html (Msg msg)
        toHiddenInput hiddenAttrs =
            Html.input hiddenAttrs []
    in
    Html.form
        ((Form.listeners formId
            |> List.map (Attr.map Form.State.FormFieldEvent)
         )
            ++ [ Attr.method (Internal.Form.methodToString options.method)
               , Attr.novalidate True

               -- TODO provide a way to override the action so users can submit to other Routes
               , Attr.action ("/" ++ String.join "/" formState.path)

               --, case options.submitStrategy of
               --     FetcherStrategy ->
               --         Pages.Internal.Msg.fetcherOnSubmit options.onSubmit formId (\_ -> isValid)
               --
               --     TransitionStrategy ->
               --         Pages.Internal.Msg.submitIfValid options.onSubmit formId (\_ -> isValid)
               ]
            ++ (let
                    formDataThing : Form.State.FormData
                    formDataThing =
                        { fields = [] -- TODO
                        , method = Form.State.Post
                        , action = formState.path |> String.join "/"
                        , id = Just formId
                        }

                    msgThing : Maybe msg
                    msgThing =
                        options.onSubmit
                            |> Maybe.map
                                (\onSubmit ->
                                    onSubmit
                                        { fields = []
                                        , parsed = parsed |> Result.fromMaybe ()
                                        }
                                )
                in
                [ Form.State.Submit formDataThing msgThing
                    |> Html.Events.onSubmit
                ]
               )
            ++ attrs
        )
        (hiddenInputs ++ children)


renderStyledHelper :
    String
    -> List (Html.Styled.Attribute (Msg msg))
    -> (actionData -> Maybe (Response error))
    -> AppContext
    -> input
    ->
        Form
            error
            { combine : Form.Validation.Validation error parsed field constraints
            , view : Context error input -> List (Html.Styled.Html (Msg msg))
            }
            parsed
            input
            msg
    -> Html.Styled.Html (Msg msg)
renderStyledHelper formId attrs accessResponse formState input ((Internal.Form.Form options _ _ _) as form) =
    -- TODO Get transition context from `app` so you can check if the current form is being submitted
    -- TODO either as a transition or a fetcher? Should be easy enough to check for the `id` on either of those?
    let
        { hiddenInputs, children, isValid, parsed } =
            helperValues formId toHiddenInput accessResponse formState input form

        toHiddenInput : List (Html.Attribute (Msg msg)) -> Html.Styled.Html (Msg msg)
        toHiddenInput hiddenAttrs =
            Html.Styled.input (hiddenAttrs |> List.map StyledAttr.fromUnstyled) []
    in
    Html.Styled.form
        ((Form.listeners formId
            |> List.map (Attr.map Form.State.FormFieldEvent)
            |> List.map StyledAttr.fromUnstyled
         )
            ++ [ StyledAttr.method (Internal.Form.methodToString options.method)
               , StyledAttr.novalidate True
               , StyledAttr.action ("/" ++ String.join "/" formState.path)

               --, Html.Events.onSubmit (options.onSubmit parsed)
               --, case options.submitStrategy of
               --     FetcherStrategy ->
               --         StyledAttr.fromUnstyled <|
               --             Pages.Internal.Msg.fetcherOnSubmit options.onSubmit formId (\_ -> isValid)
               --
               --     TransitionStrategy ->
               --         StyledAttr.fromUnstyled <|
               --             Pages.Internal.Msg.submitIfValid options.onSubmit formId (\_ -> isValid)
               ]
            ++ (let
                    formDataThing : Form.State.FormData
                    formDataThing =
                        Debug.todo ""

                    msgThing : Maybe msg
                    msgThing =
                        options.onSubmit
                            |> Maybe.map
                                (\onSubmit ->
                                    onSubmit
                                        { fields = []
                                        , parsed = parsed |> Result.fromMaybe ()
                                        }
                                )
                in
                [ Form.State.Submit formDataThing msgThing
                    |> Html.Events.onSubmit
                    |> StyledAttr.fromUnstyled
                ]
               )
            ++ attrs
        )
        (hiddenInputs ++ children)


helperValues :
    String
    -> (List (Html.Attribute (Msg msg)) -> view)
    -> (actionData -> Maybe (Response error))
    -> AppContext
    -> input
    ->
        Form
            error
            { combine : Form.Validation.Validation error parsed field constraints
            , view : Context error input -> List view
            }
            parsed
            input
            msg
    -> { hiddenInputs : List view, children : List view, isValid : Bool, parsed : Maybe parsed }
helperValues formId toHiddenInput accessResponse formState input (Internal.Form.Form _ fieldDefinitions parser toInitialValues) =
    let
        initialValues : Dict String Form.FieldState
        initialValues =
            toInitialValues input
                |> List.filterMap
                    (\( key, maybeValue ) ->
                        maybeValue
                            |> Maybe.map
                                (\value ->
                                    ( key, { value = value, status = Form.State.NotVisited } )
                                )
                    )
                |> Dict.fromList

        part2 : Dict String Form.FieldState
        part2 =
            formState.pageFormState
                |> Dict.get formId
                |> Maybe.withDefault
                    -- TODO pass in formState.action in a different way?
                    --formState.action
                    (Nothing
                        |> Maybe.andThen (accessResponse >> Maybe.map unwrapResponse)
                        |> Maybe.map
                            (\{ fields } ->
                                { fields =
                                    fields
                                        |> List.map (Tuple.mapSecond (\value -> { value = value, status = Form.State.NotVisited }))
                                        |> Dict.fromList
                                , submitAttempted = True
                                }
                            )
                        |> Maybe.withDefault initFormState
                    )
                |> .fields

        fullFormState : Dict String Form.FieldState
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
            parser (Just input) thisFormState

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
                                        -- TODO
                                        --formState.action
                                        (Nothing
                                            |> Maybe.andThen (accessResponse >> Maybe.map (unwrapResponse >> .errors))
                                            |> Maybe.withDefault Dict.empty
                                        )
                                )
                }

        thisFormState : FormState
        thisFormState =
            formState.pageFormState
                |> Dict.get formId
                |> Maybe.withDefault
                    (Nothing
                        -- TODO
                        --formState.action
                        |> Maybe.andThen (accessResponse >> Maybe.map unwrapResponse)
                        |> Maybe.map
                            (\{ fields } ->
                                { fields =
                                    fields
                                        |> List.map (Tuple.mapSecond (\value -> { value = value, status = Form.State.NotVisited }))
                                        |> Dict.fromList
                                , submitAttempted = True
                                }
                            )
                        |> Maybe.withDefault initSingle
                    )
                |> (\state -> { state | fields = fullFormState })

        context : Context error input
        context =
            { errors =
                withServerErrors
                    |> unwrapValidation
                    |> Tuple.second
                    |> Errors
            , isTransitioning = formState.isTransitioning
            , submitAttempted = thisFormState.submitAttempted
            , input = input
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
                Validation _ _ ( Just parsedValue, errors ) ->
                    Dict.isEmpty errors

                _ ->
                    False

        maybeParsed : Maybe parsed
        maybeParsed =
            case withoutServerErrors of
                Validation _ _ ( Just parsedValue, errors ) ->
                    Just parsedValue

                _ ->
                    Nothing
    in
    { hiddenInputs = hiddenInputs
    , children = children
    , isValid = isValid
    , parsed = maybeParsed
    }


initSingle : FormState
initSingle =
    { fields = Dict.empty
    , submitAttempted = False
    }


{-| -}
type alias DoneForm error parsed input view msg =
    Form
        error
        { combine : Combined error parsed
        , view : Context error input -> view
        }
        parsed
        input
        msg


{-| -}
type alias HtmlForm error parsed input msg =
    Form
        error
        { combine : Combined error parsed
        , view : Context error input -> List (Html (Msg msg))
        }
        parsed
        input
        msg


{-| -}
type alias StyledHtmlForm error parsed input msg =
    Form
        error
        { combine : Combined error parsed
        , view : Context error input -> List (Html.Styled.Html (Msg msg))
        }
        parsed
        input
        msg


{-| -}
type alias Form error combineAndView parsed input userMsg =
    Internal.Form.Form error combineAndView parsed input userMsg


{-| -}
addErrorsInternal : String -> List error -> Dict String (List error) -> Dict String (List error)
addErrorsInternal name newErrors allErrors =
    allErrors
        |> Dict.update name
            (\errors ->
                Just (newErrors ++ (errors |> Maybe.withDefault []))
            )

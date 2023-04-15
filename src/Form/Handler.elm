module Form.Handler exposing
    ( Handler
    , init, with
    , run
    )

{-| This API allows you to try parsing one of several forms.

This is useful if you want to

1.  Parse a form on the backend using code sharing to keep your backend and frontend validations in sync, and
2.  Parse in-flight form submissions to derive pending or optimistic UI state

`elm-pages` has some abstractions built around these ideas to help you receive form submissions on the backend, and
automatically manage in-flight form submissions. However, you can manually wire this in or build similar abstractions
in your own framework or application.

Example:

    import Form
    import Form.Field as Field
    import Form.Handler exposing (Handler)
    import Form.Validation as Validation

    type Action =
        UpdateProfile ( String, String )
        | SendMessage ( String, String )


    updateProfile : Form.HtmlForm String ( String, String ) input msg
    updateProfile =
        Form.form
            (\first last ->
                { combine =
                    Validation.succeed Tuple.pair
                        |> Validation.andMap first
                        |> Validation.andMap last
                , view = \_ -> []
                }
            )
            |> Form.field "first" (Field.text |> Field.required "Required")
            |> Form.field "last" (Field.text |> Field.required "Required")
            |> Form.hiddenKind ( "kind", "update-profile" ) "Expected kind"

    sendMessage : Form.HtmlForm String ( String, String ) input msg
    sendMessage =
        Form.form
            (\to body ->
                { combine =
                    Validation.succeed Tuple.pair
                        |> Validation.andMap to
                        |> Validation.andMap body
                , view = \_ -> []
                }
            )
            |> Form.field "to" (Field.text |> Field.required "Required")
            |> Form.field "body" (Field.text |> Field.required "Required" |> Field.textarea { rows = Nothing, cols = Nothing })
            |> Form.hiddenKind ( "kind", "send-message" ) "Expected kind"

    handler : Form.Handler.Handler String Action
    handler =
        Form.Handler.init UpdateProfile updateProfile
        |> Form.Handler.with SendMessage sendMessage

    Form.Handler.run
        [ ( "first", "Jane" )
        , ( "last", "Doe" )
        , ( "kind", "update-profile" )
        ]
        handler

    --> Form.Valid (UpdateProfile ("Jane", "Doe") )

    Form.Handler.run
        [ ( "to", "Jane" )
        , ( "body", "Hello!" )
        , ( "kind", "send-message" )
        ]
        handler

    --> Form.Valid (SendMessage ("Jane", "Hello!") )

@docs Handler

@docs init, with

@docs run

-}

import Dict exposing (Dict)
import Form exposing (Validated)
import Form.FieldStatus
import Form.Validation exposing (Validation)
import Internal.Form exposing (Form)
import Pages.FormState exposing (FormState)
import Pages.Internal.Form


{-| A combined set of Form parsers which can be run with [`run`](#run)
-}
type Handler error parsed
    = Handler
        (List
            (Form
                error
                (Validation error parsed Never Never)
                Never
                Never
            )
        )


{-| Start building a `Handler`.
-}
init :
    (parsed -> combined)
    ->
        Form
            error
            { combineAndView
                | combine : Validation error parsed kind constraints
            }
            parsed
            input
    -> Handler error combined
init mapFn form =
    Handler [ normalizeServerForm mapFn form ]


{-| Include an additional `Form` as one of the possibilities to parse into.
-}
with :
    (parsed -> combined)
    ->
        Form
            error
            { combineAndView
                | combine : Validation error parsed kind constraints
            }
            parsed
            input
    -> Handler error combined
    -> Handler error combined
with mapFn form (Handler serverForms) =
    Handler (serverForms ++ [ normalizeServerForm mapFn form ])



--{-| -}
--initCombinedServer :
--    (parsed -> combined)
--    ->
--        Form
--            error
--            { combineAndView
--                | combine : Combined error (BackendTask backendTaskError (Form.Validation.Validation error parsed kind constraints))
--            }
--            parsed
--            input
--            msg
--    -> ServerForms error (BackendTask backendTaskError (Form.Validation.Validation error combined kind constraints))
--initCombinedServer mapFn serverForms =
--    init (BackendTask.map (Form.Validation.map mapFn)) serverForms
--
--
--{-| -}
--combineServer :
--    (parsed -> combined)
--    ->
--        Form
--            error
--            { combineAndView
--                | combine :
--                    Combined error (BackendTask backendTaskError (Form.Validation.Validation error parsed kind constraints))
--            }
--            parsed
--            input
--            msg
--    -> ServerForms error (BackendTask backendTaskError (Form.Validation.Validation error combined kind constraints))
--    -> ServerForms error (BackendTask backendTaskError (Form.Validation.Validation error combined kind constraints))
--combineServer mapFn a b =
--    combine (BackendTask.map (Form.Validation.map mapFn)) a b


normalizeServerForm :
    (parsed -> combined)
    -> Form error { combineAndView | combine : Validation error parsed kind constraints } parsed input
    -> Form error (Validation error combined Never Never) Never Never
normalizeServerForm mapFn (Internal.Form.Form _ parseFn _) =
    Internal.Form.Form
        []
        (\_ formState ->
            let
                parsed :
                    { result : Dict String (List error)
                    , isMatchCandidate : Bool
                    , combineAndView : { combineAndView | combine : Validation error parsed kind constraints }
                    }
                parsed =
                    parseFn Nothing formState
            in
            { result = parsed.result
            , combineAndView = parsed.combineAndView.combine |> Form.Validation.map mapFn
            , isMatchCandidate = parsed.isMatchCandidate
            }
        )
        (\_ -> [])


{-| Parse your [`Handler`](#Handler) with the given raw form data into a [`Validated`](Form#Validated) value.
-}
run :
    List ( String, String )
    -> Handler error parsed
    -> Validated error parsed
run rawFormData forms =
    case runOneOfServerSideHelp rawFormData Nothing forms of
        ( Just parsed, errors ) ->
            if Dict.isEmpty errors then
                Form.Valid parsed

            else
                Form.Invalid (Just parsed) errors

        ( Nothing, errors ) ->
            Form.Invalid Nothing errors


{-| -}
runOneOfServerSideHelp :
    List ( String, String )
    -> Maybe (List ( String, List error ))
    -> Handler error parsed
    -> ( Maybe parsed, Dict String (List error) )
runOneOfServerSideHelp rawFormData firstFoundErrors (Handler parsers) =
    case parsers of
        firstParser :: remainingParsers ->
            let
                ( isMatchCandidate, thing1 ) =
                    runServerSide rawFormData firstParser

                thing : ( Maybe parsed, List ( String, List error ) )
                thing =
                    thing1
                        |> Tuple.mapSecond
                            (\errors ->
                                errors
                                    |> Dict.toList
                                    |> List.filter (Tuple.second >> List.isEmpty >> not)
                            )
            in
            case ( isMatchCandidate, thing ) of
                ( True, ( Just parsed, errors ) ) ->
                    ( Just parsed, errors |> Dict.fromList )

                ( _, ( _, errors ) ) ->
                    runOneOfServerSideHelp rawFormData
                        (firstFoundErrors
                            -- TODO is this logic what we want here? Might need to think through the semantics a bit more
                            -- of which errors to parse into - could be the first errors, the last, or some other way of
                            -- having higher precedence for deciding which form should be used
                            |> Maybe.withDefault errors
                            |> Just
                        )
                        (Handler remainingParsers)

        [] ->
            -- TODO need to pass errors
            ( Nothing, firstFoundErrors |> Maybe.withDefault [] |> Dict.fromList )


{-| -}
runServerSide :
    List ( String, String )
    -> Form error (Validation error parsed kind constraints) Never input
    -> ( Bool, ( Maybe parsed, Dict String (List error) ) )
runServerSide rawFormData (Internal.Form.Form _ parser _) =
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
            { fields =
                rawFormData
                    |> List.map
                        (Tuple.mapSecond
                            (\value ->
                                { value = value
                                , status = Form.FieldStatus.notVisited
                                }
                            )
                        )
                    |> Dict.fromList
            , submitAttempted = False
            }
    in
    ( parsed.isMatchCandidate
    , { result = ( parsed.combineAndView, parsed.result )
      }
        |> mergeResults
        |> unwrapValidation
    )


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


unwrapValidation : Validation error parsed named constraints -> ( Maybe parsed, Dict String (List error) )
unwrapValidation (Pages.Internal.Form.Validation _ _ ( maybeParsed, errors )) =
    ( maybeParsed, errors )


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


insertIfNonempty : comparable -> List value -> Dict comparable (List value) -> Dict comparable (List value)
insertIfNonempty key values dict =
    if values |> List.isEmpty then
        dict

    else
        dict
            |> Dict.insert key values

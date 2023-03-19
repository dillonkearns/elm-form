module Form.Handler exposing
    ( Handler
    , init, with
    , run
    )

{-|

@docs Handler

@docs init, with

@docs run

-}

import Dict exposing (Dict)
import Form exposing (runServerSide)
import Form.Validation exposing (Combined)
import Internal.Form exposing (Form)


{-| -}
type Handler error parsed
    = Handler
        (List
            (Form
                error
                (Combined error parsed)
                Never
                Never
                Never
            )
        )


{-| -}
init :
    (parsed -> combined)
    ->
        Form
            error
            { combineAndView
                | combine : Form.Validation.Validation error parsed kind initial constraints
            }
            parsed
            input
            msg
    -> Handler error combined
init mapFn form =
    Handler [ normalizeServerForm mapFn form ]


{-| -}
with :
    (parsed -> combined)
    ->
        Form
            error
            { combineAndView
                | combine : Form.Validation.Validation error parsed kind initial constraints
            }
            parsed
            input
            msg
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
    -> Form error { combineAndView | combine : Form.Validation.Validation error parsed kind initial constraints } parsed input msg
    -> Form error (Combined error combined) Never Never Never
normalizeServerForm mapFn (Internal.Form.Form options _ parseFn _) =
    Internal.Form.Form
        { onSubmit = Nothing
        , method = options.method
        }
        []
        (\_ formState ->
            let
                parsed :
                    { result : Dict String (List error)
                    , isMatchCandidate : Bool
                    , combineAndView : { combineAndView | combine : Form.Validation.Validation error parsed kind initial constraints }
                    }
                parsed =
                    parseFn Nothing formState
            in
            { result = parsed.result
            , combineAndView = parsed.combineAndView.combine |> Form.Validation.mapToCombined mapFn
            , isMatchCandidate = parsed.isMatchCandidate
            }
        )
        (\_ -> [])


{-| -}
run :
    List ( String, String )
    -> Handler error parsed
    -> ( Maybe parsed, Dict String (List error) )
run rawFormData forms =
    runOneOfServerSideHelp rawFormData Nothing forms


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

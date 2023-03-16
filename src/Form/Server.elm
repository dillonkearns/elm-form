module Form.Handler exposing (..)

{-| -}

import Dict exposing (Dict)
import Form.Validation exposing (Combined)
import Internal.Form exposing (Form)


type ServerForms error parsed
    = ServerForms
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
combine :
    (parsed -> combined)
    ->
        Form
            error
            { combineAndView
                | combine : Form.Validation.Validation error parsed kind constraints
            }
            parsed
            input
            msg
    -> ServerForms error combined
    -> ServerForms error combined
combine mapFn form (ServerForms serverForms) =
    ServerForms (serverForms ++ [ normalizeServerForm mapFn form ])


{-| -}
initCombinedServer :
    (parsed -> combined)
    ->
        Form
            error
            { combineAndView
                | combine : Combined error (BackendTask backendTaskError (Form.Validation.Validation error parsed kind constraints))
            }
            parsed
            input
            msg
    -> ServerForms error (BackendTask backendTaskError (Form.Validation.Validation error combined kind constraints))
initCombinedServer mapFn serverForms =
    initCombined (BackendTask.map (Form.Validation.map mapFn)) serverForms


{-| -}
combineServer :
    (parsed -> combined)
    ->
        Form
            error
            { combineAndView
                | combine :
                    Combined error (BackendTask backendTaskError (Form.Validation.Validation error parsed kind constraints))
            }
            parsed
            input
            msg
    -> ServerForms error (BackendTask backendTaskError (Form.Validation.Validation error combined kind constraints))
    -> ServerForms error (BackendTask backendTaskError (Form.Validation.Validation error combined kind constraints))
combineServer mapFn a b =
    combine (BackendTask.map (Form.Validation.map mapFn)) a b


normalizeServerForm :
    (parsed -> combined)
    -> Form error { combineAndView | combine : Form.Validation.Validation error parsed kind constraints } parsed input msg
    -> Form error (Combined error combined) Never Never Never
normalizeServerForm mapFn (Form options _ parseFn _) =
    Form
        { onSubmit = Nothing
        , submitStrategy = options.submitStrategy
        , method = options.method
        }
        []
        (\_ formState ->
            let
                parsed :
                    { result : Dict String (List error)
                    , isMatchCandidate : Bool
                    , combineAndView : { combineAndView | combine : Form.Validation.Validation error parsed kind constraints }
                    }
                parsed =
                    parseFn Nothing formState
            in
            { result = parsed.result
            , combineAndView = parsed.combineAndView.combine |> Form.Validation.mapWithNever mapFn
            , isMatchCandidate = parsed.isMatchCandidate
            }
        )
        (\_ -> [])


{-| -}
initCombined :
    (parsed -> combined)
    ->
        Form
            error
            { combineAndView
                | combine : Form.Validation.Validation error parsed kind constraints
            }
            parsed
            input
            msg
    -> ServerForms error combined
initCombined mapFn form =
    ServerForms [ normalizeServerForm mapFn form ]

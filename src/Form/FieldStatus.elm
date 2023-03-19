module Form.FieldStatus exposing (FieldStatus(..), fieldStatusToString)

{-|

@docs FieldStatus, fieldStatusToString

-}


{-| -}
type FieldStatus
    = NotVisited
    | Focused
    | Changed
    | Blurred


{-| -}
fieldStatusToString : FieldStatus -> String
fieldStatusToString fieldStatus =
    case fieldStatus of
        NotVisited ->
            "NotVisited"

        Focused ->
            "Focused"

        Changed ->
            "Changed"

        Blurred ->
            "Blurred"


{-| -}
increaseStatusTo : FieldStatus -> FieldStatus -> FieldStatus
increaseStatusTo increaseTo currentStatus =
    if statusRank increaseTo > statusRank currentStatus then
        increaseTo

    else
        currentStatus


{-| -}
statusRank : FieldStatus -> Int
statusRank status =
    case status of
        NotVisited ->
            0

        Focused ->
            1

        Changed ->
            2

        Blurred ->
            3

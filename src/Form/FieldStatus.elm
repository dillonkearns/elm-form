module Form.FieldStatus exposing
    ( FieldStatus
    , blurred
    , changed
    , focused
    , notVisited
    )


type alias FieldStatus =
    Int


fieldStatusToString : FieldStatus -> String
fieldStatusToString fieldStatus =
    case fieldStatus of
        0 ->
            "NotVisited"

        1 ->
            "Focused"

        2 ->
            "Changed"

        _ ->
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
    status


{-| -}
notVisited : Int
notVisited =
    0


{-| -}
focused : Int
focused =
    1


{-| -}
changed : Int
changed =
    2


{-| -}
blurred : Int
blurred =
    3

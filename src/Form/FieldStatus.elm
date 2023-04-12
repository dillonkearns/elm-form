module Form.FieldStatus exposing
    ( FieldStatus
    , notVisited
    )


type alias FieldStatus =
    Int


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

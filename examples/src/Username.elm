module Username exposing (Username, fromString)


type Username
    = Username String


fromString : String -> Result String Username
fromString string =
    if string |> String.contains "@" then
        Err "Must not contain @"

    else
        Username string |> Ok


toString : Username -> String
toString (Username username) =
    username

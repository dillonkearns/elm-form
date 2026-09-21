module Form.Field.Selection exposing
    ( Selection
    , value, cursorPosition, cursorAtEnd
    , zipper, SelectionZipper(..)
    )

{-| When formatting a field as the user types (e.g. an expiration date `12 / 25` or a phone number
`(555) 123-4567`), you need to know where the cursor is. Without it, reformatting the value can
jump the cursor to the end, which breaks editing when the user goes back to fix a typo.

`Selection` gives you the field value _and_ the cursor position so you can decide whether it's
safe to reformat. The most common pattern is to only format when the cursor is at the end:

    import Form.Field as Field exposing (EventInfo(..))
    import Form.Field.Selection as Selection

    Field.text
        |> Field.formatOnEvent
            (\event ->
                case event of
                    Input selection ->
                        if Selection.cursorAtEnd selection then
                            Just (formatExpDate (Selection.value selection))
                        else
                            Nothing

                    _ ->
                        Nothing
            )

You get a `Selection` from the `Input` variant of [`EventInfo`](Form-Field#EventInfo).

@docs Selection

@docs value, cursorPosition, cursorAtEnd


## Zipper Representation

For more advanced formatting where you need to know what's before and after the cursor:

@docs zipper, SelectionZipper

-}

import Internal.Selection


{-| The field value plus cursor position. You get this from the `Input` variant of
[`EventInfo`](Form-Field#EventInfo) when using [`formatOnEvent`](Form-Field#formatOnEvent).
-}
type alias Selection =
    Internal.Selection.Selection


{-| The full string value of the field. Use this to read what the user has typed so far.
-}
value : Selection -> String
value (Internal.Selection.Selection val _) =
    val


{-| Raw cursor/selection position as `( selectionStart, selectionEnd )`.

  - `( start, Nothing )` - cursor at position `start`, no text selected
  - `( start, Just end )` - text selected from `start` to `end`

Most of the time [`cursorAtEnd`](#cursorAtEnd) is all you need.

-}
cursorPosition : Selection -> ( Int, Maybe Int )
cursorPosition (Internal.Selection.Selection _ pos) =
    pos


{-| `True` when the cursor is at the end with nothing selected. This is usually when it's
safe to reformat without messing up the user's cursor position:

    Input selection ->
        if Selection.cursorAtEnd selection then
            Just (format (Selection.value selection))
        else
            Nothing

-}
cursorAtEnd : Selection -> Bool
cursorAtEnd (Internal.Selection.Selection val ( start, maybeEnd )) =
    case maybeEnd of
        Nothing ->
            start == String.length val

        Just end ->
            start == end && end == String.length val


{-| Splits the field value into parts around the cursor or selection.

  - `Cursor { before, after }` - nothing selected, cursor sits between `before` and `after`
  - `Range { before, selected, after }` - user has highlighted `selected`

-}
type SelectionZipper
    = Cursor { before : String, after : String }
    | Range { before : String, selected : String, after : String }


{-| Break the selection into a [`SelectionZipper`](#SelectionZipper).

Useful when you need to know what's on either side of the cursor, like checking if the text
before the cursor already ends with a separator before inserting another one.

-}
zipper : Selection -> SelectionZipper
zipper (Internal.Selection.Selection val ( start, maybeEnd )) =
    case maybeEnd of
        Nothing ->
            Cursor
                { before = String.left start val
                , after = String.dropLeft start val
                }

        Just end ->
            if start == end then
                Cursor
                    { before = String.left start val
                    , after = String.dropLeft start val
                    }

            else
                Range
                    { before = String.left start val
                    , selected = String.slice start end val
                    , after = String.dropLeft end val
                    }

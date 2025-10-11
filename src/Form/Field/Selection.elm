module Form.Field.Selection exposing
    ( Selection
    , value, cursorPosition, cursorAtEnd
    , zipper, SelectionZipper(..)
    )

{-|

@docs Selection


# Accessing Data

@docs value, cursorPosition, cursorAtEnd


# Zipper Representation

@docs zipper, SelectionZipper

-}

import Internal.Selection


{-| A `Selection` is the value of a form field along with its cursor position or text selection.

This is an opaque type - you cannot construct Selections directly. They are provided by the
`formatOnEvent` callback when handling form events.

-}
type alias Selection =
    Internal.Selection.Selection


{-| Get the full field value without any selection information.

    value selection
    --> "Hello World"

-}
value : Selection -> String
value (Internal.Selection.Selection val _) =
    val


{-| Get raw cursor/selection position as (selectionStart, selectionEnd).

  - If selectionEnd is `Nothing`, there is only a cursor (no selection range)
  - If selectionEnd is `Just end`, text is selected from start to end

-}
cursorPosition : Selection -> ( Int, Maybe Int )
cursorPosition (Internal.Selection.Selection _ pos) =
    pos


{-| Check if the cursor is at the end of the input (common case for auto-formatting).

Returns `True` if:

  - There is no text selection (just a cursor)
  - The cursor is positioned after the last character

-}
cursorAtEnd : Selection -> Bool
cursorAtEnd (Internal.Selection.Selection val ( start, maybeEnd )) =
    case maybeEnd of
        Nothing ->
            start == String.length val

        Just end ->
            start == end && end == String.length val


{-| A zipper representation that splits the field value based on cursor/selection position.
-}
type SelectionZipper
    = Cursor { before : String, after : String }
    | Range { before : String, selected : String, after : String }


{-| Convert selection to a structured zipper representation.

For a cursor position:

    zipper (Selection "Hello World" ( 5, Nothing ))
    --> Cursor { before = "Hello", after = " World" }

For a text selection:

    zipper (Selection "Hello World" ( 0, Just 5 ))
    --> Range { before = "", selected = "Hello", after = " World" }

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

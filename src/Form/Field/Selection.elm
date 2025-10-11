module Form.Field.Selection exposing
    ( Selection
    , fromRaw
    , value, cursorPosition, cursorAtEnd
    , zipper, SelectionZipper(..)
    )

{-|

@docs Selection
@docs fromRaw


# Accessing Data

@docs value, cursorPosition, cursorAtEnd


# Zipper Representation

@docs zipper, SelectionZipper

-}


{-| A `Selection` is the value of a form field along with its cursor position or text selection.
-}
type Selection
    = Selection String ( Int, Maybe Int )


{-| Create a Selection from raw values. This is used internally by the form decoder.
-}
fromRaw : String -> ( Int, Maybe Int ) -> Selection
fromRaw val pos =
    Selection val pos


{-| Get the full field value without any selection information.

    value selection
    --> "Hello World"

-}
value : Selection -> String
value (Selection val _) =
    val


{-| Get raw cursor/selection position as (selectionStart, selectionEnd).

  - If selectionEnd is `Nothing`, there is only a cursor (no selection range)
  - If selectionEnd is `Just end`, text is selected from start to end

-}
cursorPosition : Selection -> ( Int, Maybe Int )
cursorPosition (Selection _ pos) =
    pos


{-| Check if the cursor is at the end of the input (common case for auto-formatting).

Returns `True` if:

  - There is no text selection (just a cursor)
  - The cursor is positioned after the last character

-}
cursorAtEnd : Selection -> Bool
cursorAtEnd (Selection val ( start, maybeEnd )) =
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
zipper (Selection val ( start, maybeEnd )) =
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

module Internal.Selection exposing (Selection(..))

{-| Internal module for Selection type.
This allows internal code to construct Selections while keeping the type opaque in the public API.
-}


{-| A `Selection` is the value of a form field along with its cursor position or text selection.
-}
type Selection
    = Selection String ( Int, Maybe Int )

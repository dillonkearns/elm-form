module Form.Validation exposing
    ( Field, Validation
    , succeed
    , andMap
    , andThen, fail, fromResult, map, map2, withError, withErrorIf, withFallback
    , value, fieldName
    , FieldStatus(..), fieldStatus, fieldStatusToString
    , statusAtLeast
    , map3, map4, map5, map6, map7, map8, map9
    , global
    )

{-|


## Validations

@docs Field, Validation

@docs succeed

@docs andMap
@docs andThen, fail, fromResult, map, map2, withError, withErrorIf, withFallback


## Field Metadata

@docs value, fieldName

@docs FieldStatus, fieldStatus, fieldStatusToString

@docs statusAtLeast


## Mapping

@docs map3, map4, map5, map6, map7, map8, map9


## Global Validation

@docs global

-}

import Dict exposing (Dict)
import Pages.Internal.Form exposing (ViewField)


{-| Represents a form field. This value is used for two purposes:

  - Render - Turn the `Field` into a view using [`Form.FieldView`](Form-FieldView)
  - Parse - Use the `Field` to compose together with other `Field`s into a Validation which can be used to parse into the desired data type or add validation errors

You get a `Field` from the pipeline of fields defined using [`Form.field`](Form#field) and [`Form.hiddenField`](Form#hiddenField).
The only other way to get a `Field` is to use [`global`](#global). You can use this special `Field` to attach validation errors
to the form as a whole rather than to a specific field.

This type is one of the key designs in this package. Because we can share this `Field` type between the view and the parsing,
we don't need to use "stringly typed" values to refer to fields in our form in an error-prone way. Instead, we know that a
`Field` represents a form field that we have defined for the given [`Form`](Form#Form). It's a little confusing that this `Field`
type serves two purposes, but it allows us to have a safe way to reference a Form's fields, and doesn't require two
declarations with a duplicate parameter list.

See the [`Form`](Form) docs for more on how to use `Field`s to render your `Form`'s view and define its `combine` `Validation`.

-}
type alias Field error parsed kind =
    Validation error parsed kind { field : kind }


{-| A `Validation` represents a parsed value or a list of errors.
-}
type alias Validation error parsed kind constraints =
    Pages.Internal.Form.Validation error parsed kind constraints


{-| Get the `Field`'s name.
-}
fieldName : Field error parsed kind -> String
fieldName (Pages.Internal.Form.Validation _ name _) =
    name
        |> Maybe.withDefault ""


{-| Get the `Field`'s [`FieldStatus`](#FieldStatus).
-}
fieldStatus : Field error parsed kind -> FieldStatus
fieldStatus (Pages.Internal.Form.Validation viewField _ _) =
    viewField
        |> Maybe.map .status
        |> Maybe.withDefault 0
        |> statusFromRank


{-| Mostly useful for debugging, you'll usually want to compare `FieldStatus` to other `FieldStatus` values or use helpers like [`statusAtLeast`](#statusAtLeast).
-}
fieldStatusToString : FieldStatus -> String
fieldStatusToString status =
    case status of
        NotVisited ->
            "NotVisited"

        Focused ->
            "Focused"

        Changed ->
            "Changed"

        Blurred ->
            "Blurred"


{-| `elm-form` manages form fields' `FieldStatus` in the order described in [`FieldStatus`](#FieldStatus).

This function is useful when writing a condition for when to show a `Field`'s validation errors in the view.
See [`Form.errorsForField`](Form#errorsForField) for an example.

-}
statusAtLeast : FieldStatus -> Field error parsed kind -> Bool
statusAtLeast status field =
    (field |> fieldStatus |> statusRank) >= statusRank status


{-| `elm-form` manages the state of a form's fields, including `FieldStatus`.

`FieldStatus` goes through the following states in this order, and never goes backwards (unless it is changed manually through the [`Form.Model`](Form#Model) value):

1.  `NotVisited` - The initial `FieldStatus` for all fields. The field hasn't been changed or focused.
2.  `Focused` - The field has been focused, but not changed or blurred.
3.  `Changed` - The field has had an input event, but has not yet been blurred (lost focus).
4.  `Blurred` - The field has been blurred (lost focus).

You can use a [`Form`](Form#Form)'s `submitAttempted` state and/or a `Field`'s `FieldStatus` to decide when to render a `Field`'s validation errors.

For example, you might choose to render a `Field`'s validation errors once `Field` has been `Blurred` so that the user doesn't see validation errors before they've had a chance
to input a valid value (it can be an annoying user experience to get error feedback too early).

See [`Form.errorsForField`](Form#errorsForField) for a common way to use a `Field`'s `FieldStatus`.

-}
type FieldStatus
    = NotVisited
    | Focused
    | Changed
    | Blurred


statusFromRank : Int -> FieldStatus
statusFromRank int =
    case int of
        0 ->
            NotVisited

        1 ->
            Focused

        2 ->
            Changed

        3 ->
            Blurred

        _ ->
            Blurred


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


expectViewField : Maybe (ViewField kind) -> ViewField kind
expectViewField viewField =
    case viewField of
        Just justViewField ->
            justViewField

        Nothing ->
            expectViewField viewField


{-| Get a `Combined` value that successfully parses into the given value with no errors.

Helpful for starting a chain of `Validation` functions that will eventually parse into a value. See [`andMap`](#andMap)
for an example of a common idiom.

-}
succeed : parsed -> Validation error parsed kind constraints
succeed parsed =
    Pages.Internal.Form.Validation Nothing Nothing ( Just parsed, Dict.empty )


{-| A `Field` that represents the form as a whole. This is useful for attaching validation errors to the form as a whole rather than to a specific field.
-}
global : Field error () Never
global =
    Pages.Internal.Form.Validation Nothing
        (Just "$$global$$")
        ( Just ()
        , Dict.empty
        )


{-| Include a fallback value to parse into. Does not remove any previous validation errors that have been encountered
so it will not effect whether it parses to `Valid` or `Invalid`.
-}
withFallback : parsed -> Validation error parsed named constraints -> Validation error parsed named constraints
withFallback parsed (Pages.Internal.Form.Validation viewField name ( maybeParsed, errors )) =
    Pages.Internal.Form.Validation viewField
        name
        ( maybeParsed
            |> Maybe.withDefault parsed
            |> Just
        , errors
        )


{-| Get the parsed value if it is parseable (could be either invalid or valid).
-}
value : Validation error parsed named constraint -> Maybe parsed
value (Pages.Internal.Form.Validation _ _ ( maybeParsed, _ )) =
    maybeParsed


{-| Add an error to the given `Field`, short-circuiting its parsed value so that it will fail to parse.
This can be helpful if you want to fail with a value that you can combine together with other values because
it has an unbound `parsed` type variable. Similar to how [`Json.Decode.fail`](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#fail)
gives you a `Decoder a`.

See [`withError`](#withError) if you want to add an error without short-circuiting the parsed value.

-}
fail : error -> Field error parsed1 field -> Validation error parsed kind constraints
fail parsed (Pages.Internal.Form.Validation _ key _) =
    Pages.Internal.Form.Validation Nothing Nothing ( Nothing, Dict.singleton (key |> Maybe.withDefault "") [ parsed ] )


{-| Add an error to the given `Field`.
-}
withError : Field error parsed1 field -> error -> Validation error parsed2 named constraints -> Validation error parsed2 named constraints
withError (Pages.Internal.Form.Validation _ key _) error (Pages.Internal.Form.Validation viewField name ( maybeParsedA, errorsA )) =
    Pages.Internal.Form.Validation viewField name ( maybeParsedA, errorsA |> insertIfNonempty (key |> Maybe.withDefault "") [ error ] )


{-| Conditionally add an error to the given `Field`.

    import Date
    import Form.Validation as Validation

    example checkIn checkOut =
        Validation.map2
            (\checkinValue checkoutValue ->
                Validation.succeed
                    { date = checkinValue
                    , nights = Date.toRataDie checkoutValue - Date.toRataDie checkinValue
                    }
                    |> Validation.withErrorIf (Date.toRataDie checkinValue >= Date.toRataDie checkoutValue) checkIn "Must be before checkout"
            )
            checkIn
            checkOut
            |> Validation.andThen identity

-}
withErrorIf : Bool -> Field error ignored field -> error -> Validation error parsed named constraints -> Validation error parsed named constraints
withErrorIf includeError (Pages.Internal.Form.Validation _ key _) error (Pages.Internal.Form.Validation viewField name ( maybeParsedA, errorsA )) =
    Pages.Internal.Form.Validation viewField
        name
        ( maybeParsedA
        , if includeError then
            errorsA |> insertIfNonempty (key |> Maybe.withDefault "") [ error ]

          else
            errorsA
        )


{-| Apply a function to the parsed value.

    import Form.Validation as Validation

    type Uuid
        = Uuid String

    example =
        (\uuid ->
            { combine =
                Validation.succeed identity
                    |> Validation.andMap (uuid |> Validation.map Uuid)
            , view = \_ -> []
            }
        )
            |> Form.form
            |> Form.hiddenField "uuid" (Field.text |> Field.required "Required")

-}
map : (parsed -> mapped) -> Validation error parsed named constraint -> Validation error mapped erasedKind erasedConstraints
map mapFn (Pages.Internal.Form.Validation _ name ( maybeParsedA, errorsA )) =
    Pages.Internal.Form.Validation Nothing name ( Maybe.map mapFn maybeParsedA, errorsA )


{-| Resolve a `Result` within a `Field`. If it is `Err`, the error will be added to the `Field`'s errors.
-}
fromResult : Field error (Result error parsed) kind -> Validation error parsed Never Never
fromResult fieldResult =
    fieldResult
        |> andThen
            (\parsedValue ->
                case parsedValue of
                    Ok okValue ->
                        succeed okValue

                    Err error ->
                        fail error fieldResult
            )


{-| Lets you combine Validation's in a pipeline.

    import Form
    import Form.Field as Field
    import Form.Validation as Validation

    example =
        (\first middle last ->
            { combine =
                Validation.succeed
                    (\vFirst vMiddle vLast ->
                        vFirst ++ " " ++ vMiddle ++ " " ++ vLast
                    )
                    |> Validation.andMap first
                    |> Validation.andMap middle
                    |> Validation.andMap last
            , view = \_ -> []
            }
        )
            |> Form.form
            |> Form.field "first" (Field.text |> Field.required "Required")
            |> Form.field "middle" (Field.text |> Field.required "Required")
            |> Form.field "last" (Field.text |> Field.required "Required")

-}
andMap : Validation error a named1 constraints1 -> Validation error (a -> b) named2 constraints2 -> Validation error b Never constraints3
andMap =
    map2 (|>)


{-| Continue a `Validation` based on the given `parsed` value.

    import Date
    import Form
    import Form.Field as Field
    import Form.Validation as Validation

    example : Form.HtmlForm String { date : Date, nights : Int } input msg
    example =
        (\checkIn checkOut ->
            { combine =
                Validation.map2
                    (\checkinValue checkoutValue ->
                        Validation.succeed
                            { date = checkinValue
                            , nights = Date.toRataDie checkoutValue - Date.toRataDie checkinValue
                            }
                            |> Validation.withErrorIf (Date.toRataDie checkinValue >= Date.toRataDie checkoutValue) checkIn "Must be before checkout"
                    )
                    checkIn
                    checkOut
                    |> Validation.andThen identity
            , view = \_ -> []
            }
        )
            |> Form.form
            |> Form.field "checkin"
                (Field.date
                    { invalid = \_ -> "Invalid" }
                    |> Field.required "Required"
                )
            |> Form.field "checkout"
                (Field.date
                    { invalid = \_ -> "Invalid" }
                    |> Field.required "Required"
                )

-}
andThen : (parsed -> Validation error mapped named1 constraints1) -> Validation error parsed named2 constraints2 -> Validation error mapped Never constraintsAny
andThen andThenFn (Pages.Internal.Form.Validation _ _ ( maybeParsed, errors )) =
    case maybeParsed of
        Just parsed ->
            andThenFn parsed
                |> (\(Pages.Internal.Form.Validation _ _ ( andThenParsed, andThenErrors )) ->
                        Pages.Internal.Form.Validation Nothing Nothing ( andThenParsed, mergeErrors errors andThenErrors )
                   )

        Nothing ->
            Pages.Internal.Form.Validation Nothing Nothing ( Nothing, errors )


{-| Combine together two `Validation`'s.

    import Form
    import Form.Field as Field
    import Form.Validation as Validation

    example =
        (\first last ->
            { combine =
                Validation.map2 Tuple.pair
                    first
                    last
            , view = \_ -> [{- ... -}]
            }
        )
            |> Form.form
            |> Form.field "first" (Field.text |> Field.required "Required")
            |> Form.field "last" (Field.text |> Field.required "Required")

-}
map2 : (a -> b -> c) -> Validation error a named1 constraints1 -> Validation error b named2 constraints2 -> Validation error c Never constraintsAny
map2 f (Pages.Internal.Form.Validation _ _ ( maybeParsedA, errorsA )) (Pages.Internal.Form.Validation _ _ ( maybeParsedB, errorsB )) =
    Pages.Internal.Form.Validation Nothing
        Nothing
        ( Maybe.map2 f maybeParsedA maybeParsedB
        , mergeErrors errorsA errorsB
        )


{-| -}
map3 :
    (a1 -> a2 -> a3 -> a4)
    -> Validation error a1 named1 constraints1
    -> Validation error a2 named2 constraints2
    -> Validation error a3 named3 constraints3
    -> Validation error a4 Never constraintsAny
map3 f validation1 validation2 validation3 =
    succeed f
        |> andMap validation1
        |> andMap validation2
        |> andMap validation3


{-| -}
map4 :
    (a1 -> a2 -> a3 -> a4 -> a5)
    -> Validation error a1 named1 constraints1
    -> Validation error a2 named2 constraints2
    -> Validation error a3 named3 constraints3
    -> Validation error a4 named4 constraints4
    -> Validation error a5 Never constraintsAny
map4 f validation1 validation2 validation3 validation4 =
    succeed f
        |> andMap validation1
        |> andMap validation2
        |> andMap validation3
        |> andMap validation4


{-| -}
map5 :
    (a1 -> a2 -> a3 -> a4 -> a5 -> a6)
    -> Validation error a1 named1 constraints1
    -> Validation error a2 named2 constraints2
    -> Validation error a3 named3 constraints3
    -> Validation error a4 named4 constraints4
    -> Validation error a5 named5 constraints5
    -> Validation error a6 Never constraintsAny
map5 f validation1 validation2 validation3 validation4 validation5 =
    succeed f
        |> andMap validation1
        |> andMap validation2
        |> andMap validation3
        |> andMap validation4
        |> andMap validation5


{-| -}
map6 :
    (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7)
    -> Validation error a1 named1 constraints1
    -> Validation error a2 named2 constraints2
    -> Validation error a3 named3 constraints3
    -> Validation error a4 named4 constraints4
    -> Validation error a5 named5 constraints5
    -> Validation error a6 named6 constraints6
    -> Validation error a7 Never constraintsAny
map6 f validation1 validation2 validation3 validation4 validation5 validation6 =
    succeed f
        |> andMap validation1
        |> andMap validation2
        |> andMap validation3
        |> andMap validation4
        |> andMap validation5
        |> andMap validation6


{-| -}
map7 :
    (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8)
    -> Validation error a1 named1 constraints1
    -> Validation error a2 named2 constraints2
    -> Validation error a3 named3 constraints3
    -> Validation error a4 named4 constraints4
    -> Validation error a5 named5 constraints5
    -> Validation error a6 named6 constraints6
    -> Validation error a7 named7 constraints7
    -> Validation error a8 Never constraintsAny
map7 f validation1 validation2 validation3 validation4 validation5 validation6 validation7 =
    succeed f
        |> andMap validation1
        |> andMap validation2
        |> andMap validation3
        |> andMap validation4
        |> andMap validation5
        |> andMap validation6
        |> andMap validation7


{-| -}
map8 :
    (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9)
    -> Validation error a1 named1 constraints1
    -> Validation error a2 named2 constraints2
    -> Validation error a3 named3 constraints3
    -> Validation error a4 named4 constraints4
    -> Validation error a5 named5 constraints5
    -> Validation error a6 named6 constraints6
    -> Validation error a7 named7 constraints7
    -> Validation error a8 named8 constraints8
    -> Validation error a9 Never constraintsAny
map8 f validation1 validation2 validation3 validation4 validation5 validation6 validation7 validation8 =
    succeed f
        |> andMap validation1
        |> andMap validation2
        |> andMap validation3
        |> andMap validation4
        |> andMap validation5
        |> andMap validation6
        |> andMap validation7
        |> andMap validation8


{-| -}
map9 :
    (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10)
    -> Validation error a1 named1 constraints1
    -> Validation error a2 named2 constraints2
    -> Validation error a3 named3 constraints3
    -> Validation error a4 named4 constraints4
    -> Validation error a5 named5 constraints5
    -> Validation error a6 named6 constraints6
    -> Validation error a7 named7 constraints7
    -> Validation error a8 named8 constraints8
    -> Validation error a9 named9 constraints9
    -> Validation error a10 Never constraintsAny
map9 f validation1 validation2 validation3 validation4 validation5 validation6 validation7 validation8 validation9 =
    succeed f
        |> andMap validation1
        |> andMap validation2
        |> andMap validation3
        |> andMap validation4
        |> andMap validation5
        |> andMap validation6
        |> andMap validation7
        |> andMap validation8
        |> andMap validation9


{-| -}
mergeErrors : Dict comparable (List value) -> Dict comparable (List value) -> Dict comparable (List value)
mergeErrors errors1 errors2 =
    Dict.merge
        (\key entries soFar ->
            soFar |> insertIfNonempty key entries
        )
        (\key entries1 entries2 soFar ->
            soFar |> insertIfNonempty key (entries1 ++ entries2)
        )
        (\key entries soFar ->
            soFar |> insertIfNonempty key entries
        )
        errors1
        errors2
        Dict.empty


{-| -}
insertIfNonempty : comparable -> List value -> Dict comparable (List value) -> Dict comparable (List value)
insertIfNonempty key values dict =
    if values |> List.isEmpty then
        dict

    else
        dict
            |> Dict.insert key values

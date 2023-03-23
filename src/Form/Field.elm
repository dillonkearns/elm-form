module Form.Field exposing
    ( Field
    , text, checkbox, int, float
    , select, OutsideRange(..)
    , date, time, TimeOfDay
    , withInitialValue, withOptionalInitialValue
    , exactValue
    , required, withClientValidation, map
    , email, password, search, telephone, url, textarea
    , range, withMin, withMax
    , withMinLength, withMaxLength
    , withStep, withFloatStep
    , withOnChange
    , No, Yes
    )

{-|

@docs Field


## Base Fields

@docs text, checkbox, int, float


## Multiple Choice Fields

@docs select, OutsideRange


## Date/Time Fields

@docs date, time, TimeOfDay


## Initial Values

@docs withInitialValue, withOptionalInitialValue


## Other

@docs exactValue


## Field Configuration

@docs required, withClientValidation, map


## Text Field Display Options

@docs email, password, search, telephone, url, textarea


## Numeric Field Options

@docs range, withMin, withMax

@docs withMinLength, withMaxLength

@docs withStep, withFloatStep


## Events

@docs withOnChange


## Phantom Options

@docs No, Yes

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Form.FieldView exposing (Input)
import Internal.Field
import Internal.FieldEvent exposing (Event(..))
import Internal.Input exposing (Options(..))
import Json.Encode as Encode


{-| -}
type alias Field error parsed input initial kind msg constraints =
    Internal.Field.Field error parsed input initial kind msg constraints


{-| -}
type Yes
    = Yes Never


{-| -}
type No
    = No Never


{-| -}
required :
    error
    ->
        Field
            error
            (Maybe parsed)
            kind
            input
            initial
            msg
            { constraints
                | required : ()
                , wasMapped : No
            }
    -> Field error parsed kind input initial msg { constraints | wasMapped : No }
required missingError (Internal.Field.Field field kind) =
    Internal.Field.Field
        { initialValue = field.initialValue
        , initialToString = field.initialToString
        , initialFromString = field.initialFromString
        , decode =
            \rawValue ->
                let
                    ( parsed, allErrors ) =
                        field.decode rawValue

                    isEmpty : Bool
                    isEmpty =
                        rawValue == Just "" || rawValue == Nothing
                in
                ( parsed |> Maybe.andThen identity
                , if isEmpty then
                    missingError :: allErrors

                  else
                    allErrors
                )
        , properties = field.properties
        , compare = field.compare
        , toMsg = field.toMsg
        }
        kind


{-| -}
text :
    Field
        error
        (Maybe String)
        input
        String
        Input
        msg
        { required : ()
        , plainText : ()
        , wasMapped : No
        , minlength : ()
        , maxlength : ()
        }
text =
    Internal.Field.Field
        { initialValue = \_ -> Nothing
        , initialToString = identity
        , initialFromString = \value -> Just value
        , decode =
            \rawValue ->
                ( if rawValue == Just "" then
                    Just Nothing

                  else
                    Just rawValue
                , []
                )
        , properties = []
        , compare = Basics.compare
        , toMsg = Nothing
        }
        (Internal.Input.Input Internal.Input.Text)


{-| -}
date :
    { invalid : String -> error }
    ->
        Field
            error
            (Maybe Date)
            input
            Date
            Input
            msg
            { min : Date
            , max : Date
            , required : ()
            , wasMapped : No
            , step : Int
            }
date toError =
    Internal.Field.Field
        { initialValue = \_ -> Nothing
        , initialToString = Date.toIsoString
        , initialFromString =
            \rawString ->
                rawString
                    |> Date.fromIsoString
                    |> Result.toMaybe
        , decode =
            \rawString ->
                if (rawString |> Maybe.withDefault "") == "" then
                    ( Just Nothing, [] )

                else
                    case
                        rawString
                            |> Maybe.withDefault ""
                            |> Date.fromIsoString
                            |> Result.mapError (\_ -> toError.invalid (rawString |> Maybe.withDefault ""))
                    of
                        Ok parsedDate ->
                            ( Just (Just parsedDate), [] )

                        Err error ->
                            ( Nothing, [ error ] )
        , properties = []
        , compare =
            \raw value ->
                Result.map2 Date.compare
                    (Ok value)
                    (Date.fromIsoString raw)
                    |> Result.withDefault LT
        , toMsg = Nothing
        }
        (Internal.Input.Input Internal.Input.Date)


{-| -}
type alias TimeOfDay =
    { hours : Int
    , minutes : Int
    }


{-| <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/time>
-}
time :
    { invalid : String -> error }
    ->
        Field
            error
            (Maybe TimeOfDay)
            input
            TimeOfDay
            Input
            msg
            { min : TimeOfDay
            , max : TimeOfDay
            , required : ()
            , wasMapped : No
            }
time toError =
    Internal.Field.Field
        { initialValue = \_ -> Nothing
        , initialToString = timeOfDayToString
        , initialFromString =
            \rawString ->
                rawString
                    |> parseTimeOfDay
                    |> Result.toMaybe
        , decode =
            \rawString ->
                if (rawString |> Maybe.withDefault "") == "" then
                    ( Just Nothing, [] )

                else
                    case
                        rawString
                            |> Maybe.withDefault ""
                            |> parseTimeOfDay
                            |> Result.mapError (\_ -> toError.invalid (rawString |> Maybe.withDefault ""))
                    of
                        Ok parsedDate ->
                            ( Just (Just parsedDate), [] )

                        Err error ->
                            ( Nothing, [ error ] )
        , properties = []
        , compare =
            \raw value ->
                parseTimeOfDay raw
                    |> Result.map
                        (\parsedRaw ->
                            if parsedRaw.hours == value.hours then
                                Basics.compare parsedRaw.minutes value.minutes

                            else
                                Basics.compare parsedRaw.hours value.hours
                        )
                    |> Result.withDefault LT
        , toMsg = Nothing
        }
        (Internal.Input.Input Internal.Input.Time)


timeOfDayToString : TimeOfDay -> String
timeOfDayToString { hours, minutes } =
    paddedInt hours ++ ":" ++ paddedInt minutes


paddedInt : Int -> String
paddedInt intValue =
    intValue
        |> String.fromInt
        |> String.padLeft 2 '0'


parseTimeOfDay : String -> Result () { hours : Int, minutes : Int }
parseTimeOfDay rawTimeOfDay =
    case rawTimeOfDay |> String.split ":" |> List.map String.toInt of
        [ Just hours, Just minutes, Just _ ] ->
            Ok
                { hours = hours
                , minutes = minutes
                }

        [ Just hours, Just minutes ] ->
            Ok
                { hours = hours
                , minutes = minutes
                }

        _ ->
            Err ()


{-| -}
select :
    List ( String, option )
    -> (String -> error)
    ->
        Field
            error
            (Maybe option)
            input
            option
            (Options option)
            msg
            { required : ()
            , wasMapped : No
            }
select optionsMapping invalidError =
    let
        dict : Dict String option
        dict =
            Dict.fromList optionsMapping

        fromString : String -> Maybe option
        fromString string =
            Dict.get string dict
    in
    Internal.Field.Field
        { initialValue = \_ -> Nothing
        , initialToString = enumToString optionsMapping
        , initialFromString = fromString
        , decode =
            \rawValue ->
                case rawValue of
                    Nothing ->
                        ( Just Nothing, [] )

                    Just "" ->
                        ( Just Nothing, [] )

                    Just justValue ->
                        let
                            parsed : Maybe option
                            parsed =
                                fromString justValue
                        in
                        case parsed of
                            Just okParsed ->
                                ( Just (Just okParsed)
                                , []
                                )

                            Nothing ->
                                ( Just Nothing
                                , [ invalidError justValue
                                  ]
                                )
        , properties = []
        , compare =
            \_ _ ->
                -- min/max properties aren't allowed for this field type
                EQ
        , toMsg = Nothing
        }
        (Options fromString (optionsMapping |> List.map Tuple.first))


enumToString : List ( String, enum ) -> enum -> String
enumToString optionsMapping a =
    case optionsMapping |> List.filter (\( _, b ) -> b == a) |> List.head of
        Just ( str, _ ) ->
            str

        Nothing ->
            "Missing enum"


{-| -}
exactValue :
    String
    -> error
    ->
        Field
            error
            String
            input
            Never
            Input
            msg
            { required : ()
            , plainText : ()
            , wasMapped : No
            }
exactValue initialValue error =
    Internal.Field.Field
        { initialValue = \_ -> Nothing
        , initialToString = never
        , initialFromString = \_ -> Nothing
        , decode =
            \rawValue ->
                if rawValue == Just initialValue then
                    ( rawValue, [] )

                else
                    ( rawValue, [ error ] )
        , properties = []
        , compare =
            \_ _ ->
                -- min/max properties aren't allowed for this field type
                EQ
        , toMsg = Nothing
        }
        (Internal.Input.Input Internal.Input.Text)


{-| -}
checkbox :
    Field
        error
        Bool
        input
        Bool
        Input
        msg
        { required : ()
        }
checkbox =
    Internal.Field.Field
        { initialValue = \_ -> Nothing
        , initialToString =
            \bool ->
                if bool then
                    "on"

                else
                    ""
        , initialFromString =
            \rawString ->
                (rawString == "on") |> Just
        , decode =
            \rawString ->
                ( (rawString == Just "on")
                    |> Just
                , []
                )
        , properties = []
        , compare =
            \_ _ ->
                -- min/max properties aren't allowed for this field type
                EQ
        , toMsg = Nothing
        }
        (Internal.Input.Input Internal.Input.Checkbox)


{-| -}
int :
    { invalid : String -> error }
    ->
        Field
            error
            (Maybe Int)
            input
            Int
            Input
            msg
            { min : Int
            , max : Int
            , required : ()
            , wasMapped : No
            , step : Int
            }
int toError =
    Internal.Field.Field
        { initialValue = \_ -> Nothing
        , initialToString = String.fromInt
        , initialFromString = String.toInt
        , decode =
            \rawString ->
                case rawString of
                    Nothing ->
                        ( Just Nothing, [] )

                    Just "" ->
                        ( Just Nothing, [] )

                    Just string ->
                        case string |> String.toInt of
                            Just parsedInt ->
                                ( Just (Just parsedInt), [] )

                            Nothing ->
                                ( Nothing, [ toError.invalid string ] )
        , properties = []
        , compare =
            \raw value ->
                case String.toInt raw of
                    Just parsed ->
                        Basics.compare parsed value

                    _ ->
                        LT
        , toMsg = Nothing
        }
        (Internal.Input.Input Internal.Input.Number)


{-| -}
float :
    { invalid : String -> error }
    ->
        Field
            error
            (Maybe Float)
            input
            Float
            Input
            msg
            { min : Float
            , max : Float
            , required : ()
            , wasMapped : No
            }
float toError =
    Internal.Field.Field
        { initialValue = \_ -> Nothing
        , initialToString = String.fromFloat
        , initialFromString = String.toFloat
        , decode =
            \rawString ->
                case rawString of
                    Nothing ->
                        ( Just Nothing, [] )

                    Just "" ->
                        ( Just Nothing, [] )

                    Just string ->
                        case string |> String.toFloat of
                            Just parsedFloat ->
                                ( Just (Just parsedFloat), [] )

                            Nothing ->
                                ( Nothing, [ toError.invalid string ] )
        , properties = []
        , compare =
            \raw value ->
                case String.toFloat raw of
                    Just parsed ->
                        Basics.compare parsed value

                    _ ->
                        LT
        , toMsg = Nothing
        }
        (Internal.Input.Input Internal.Input.Number)


{-| -}
telephone :
    Field error parsed input initial Input msg { constraints | plainText : () }
    -> Field error parsed input initial Input msg constraints
telephone (Internal.Field.Field field _) =
    Internal.Field.Field field
        (Internal.Input.Input Internal.Input.Tel)


{-| -}
search :
    Field error parsed input initial Input msg { constraints | plainText : () }
    -> Field error parsed input initial Input msg constraints
search (Internal.Field.Field field _) =
    Internal.Field.Field field
        (Internal.Input.Input Internal.Input.Search)


{-| -}
password :
    Field error parsed input initial Input msg { constraints | plainText : () }
    -> Field error parsed input initial Input msg constraints
password (Internal.Field.Field field _) =
    Internal.Field.Field field
        (Internal.Input.Input Internal.Input.Password)


{-| -}
email :
    Field error parsed input initial Input msg { constraints | plainText : () }
    -> Field error parsed input initial Input msg constraints
email (Internal.Field.Field field _) =
    Internal.Field.Field field
        (Internal.Input.Input Internal.Input.Email)


{-| -}
url :
    Field error parsed input initial Input msg { constraints | plainText : () }
    -> Field error parsed input initial Input msg constraints
url (Internal.Field.Field field _) =
    Internal.Field.Field field
        (Internal.Input.Input Internal.Input.Url)


{-| -}
textarea :
    { rows : Maybe Int, cols : Maybe Int }
    -> Field error parsed input initial Input msg { constraints | plainText : () }
    -> Field error parsed input initial Input msg constraints
textarea options (Internal.Field.Field field _) =
    Internal.Field.Field field (Internal.Input.Input (Internal.Input.Textarea options))


{-| -}
type OutsideRange
    = AboveRange
    | BelowRange


{-| -}
range :
    { min : numberInitial
    , max : numberInitial
    , missing : error
    , invalid : OutsideRange -> error
    }
    ->
        Field
            error
            (Maybe valueType)
            input
            numberInitial
            kind
            msg
            { constraints
                | required : ()
                , min : numberInitial
                , max : numberInitial
                , wasMapped : No
            }
    ->
        Field
            error
            valueType
            input
            numberInitial
            Input
            msg
            { constraints | wasMapped : No }
range info field =
    -- TODO set the default value (if not overridden) using this https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/range#value
    field
        |> required info.missing
        |> withMin info.min (info.invalid BelowRange)
        |> withMax info.max (info.invalid AboveRange)
        |> (\(Internal.Field.Field innerField _) -> Internal.Field.Field innerField (Internal.Input.Input Internal.Input.Range))


{-| -}
map : (parsed -> mapped) -> Field error parsed input initial kind msg constraints -> Field error mapped input initial kind msg { constraints | wasMapped : Yes }
map mapFn field_ =
    withClientValidation
        (\value -> ( Just (mapFn value), [] ))
        field_


{-| -}
withClientValidation : (parsed -> ( Maybe mapped, List error )) -> Field error parsed input initial kind msg constraints -> Field error mapped input initial kind msg { constraints | wasMapped : Yes }
withClientValidation mapFn (Internal.Field.Field field kind) =
    Internal.Field.Field
        { initialValue = field.initialValue
        , initialToString = field.initialToString
        , initialFromString = field.initialFromString
        , decode =
            \value ->
                value
                    |> field.decode
                    |> (\( maybeValue, errors ) ->
                            case maybeValue of
                                Nothing ->
                                    ( Nothing, errors )

                                Just okValue ->
                                    okValue
                                        |> mapFn
                                        |> Tuple.mapSecond ((++) errors)
                       )
        , properties = field.properties
        , compare = field.compare
        , toMsg = field.toMsg
        }
        kind


{-| -}
withMin : initial -> error -> Field error parsed input initial kind msg { constraints | min : initial } -> Field error parsed input initial kind msg constraints
withMin min error (Internal.Field.Field field kind) =
    Internal.Field.Field
        { initialValue = field.initialValue
        , initialToString = field.initialToString
        , initialFromString = field.initialFromString
        , decode =
            \value ->
                value
                    |> field.decode
                    |> (\( maybeValue, errors ) ->
                            case maybeValue of
                                Nothing ->
                                    ( Nothing, errors )

                                Just okValue ->
                                    if isEmptyValue value then
                                        ( Just okValue, errors )

                                    else
                                        case field.compare (value |> Maybe.withDefault "") min of
                                            LT ->
                                                ( Just okValue, error :: errors )

                                            _ ->
                                                ( Just okValue, errors )
                       )
        , properties = ( "min", Encode.string (field.initialToString min) ) :: field.properties
        , compare = field.compare
        , toMsg = field.toMsg
        }
        kind


{-| -}
withMinLength : Int -> error -> Field error parsed input initial kind msg { constraints | minlength : () } -> Field error parsed input initial kind msg constraints
withMinLength minLength error (Internal.Field.Field field kind) =
    Internal.Field.Field
        { initialValue = field.initialValue
        , initialToString = field.initialToString
        , initialFromString = field.initialFromString
        , decode =
            \value ->
                value
                    |> field.decode
                    |> (\( maybeValue, errors ) ->
                            case maybeValue of
                                Nothing ->
                                    ( Nothing, errors )

                                Just okValue ->
                                    if (value |> Maybe.withDefault "" |> String.length) >= minLength then
                                        ( Just okValue, errors )

                                    else
                                        ( Just okValue, error :: errors )
                       )
        , properties = ( "minlength", Encode.string (String.fromInt minLength) ) :: field.properties
        , compare = field.compare
        , toMsg = field.toMsg
        }
        kind


{-| -}
withMaxLength : Int -> error -> Field error parsed input initial kind msg { constraints | maxlength : () } -> Field error parsed input initial kind msg constraints
withMaxLength maxLength error (Internal.Field.Field field kind) =
    Internal.Field.Field
        { initialValue = field.initialValue
        , initialToString = field.initialToString
        , initialFromString = field.initialFromString
        , decode =
            \value ->
                value
                    |> field.decode
                    |> (\( maybeValue, errors ) ->
                            case maybeValue of
                                Nothing ->
                                    ( Nothing, errors )

                                Just okValue ->
                                    if (value |> Maybe.withDefault "" |> String.length) <= maxLength then
                                        ( Just okValue, errors )

                                    else
                                        ( Just okValue, error :: errors )
                       )
        , properties = ( "maxlength", Encode.string (String.fromInt maxLength) ) :: field.properties
        , compare = field.compare
        , toMsg = field.toMsg
        }
        kind


isEmptyValue : Maybe String -> Bool
isEmptyValue value =
    (value |> Maybe.withDefault "") == ""


{-| -}
withMax : initial -> error -> Field error parsed input initial kind msg { constraints | max : initial } -> Field error parsed input initial kind msg constraints
withMax max error (Internal.Field.Field field kind) =
    Internal.Field.Field
        { initialValue = field.initialValue
        , initialToString = field.initialToString
        , initialFromString = field.initialFromString
        , decode =
            \value ->
                value
                    |> field.decode
                    |> (\( maybeValue, errors ) ->
                            case maybeValue of
                                Nothing ->
                                    ( Nothing, errors )

                                Just okValue ->
                                    if isEmptyValue value then
                                        ( Just okValue, errors )

                                    else
                                        case field.compare (value |> Maybe.withDefault "") max of
                                            GT ->
                                                ( Just okValue, error :: errors )

                                            _ ->
                                                ( Just okValue, errors )
                       )
        , properties = ( "max", Encode.string (field.initialToString max) ) :: field.properties
        , compare = field.compare
        , toMsg = field.toMsg
        }
        kind


{-| -}
withStep : Int -> Field error value input initial view msg { constraints | step : Int } -> Field error value input initial view msg constraints
withStep step (Internal.Field.Field info kind) =
    withStringProperty ( "step", String.fromInt step ) (Internal.Field.Field info kind)


{-| -}
withFloatStep : Float -> Field error value input initial view msg { constraints | step : Float } -> Field error value input initial view msg constraints
withFloatStep step (Internal.Field.Field info kind) =
    withStringProperty ( "step", String.fromFloat step ) (Internal.Field.Field info kind)


withStringProperty : ( String, String ) -> Field error parsed input initial kind msg constraints1 -> Field error parsed input initial kind msg constraints2
withStringProperty ( key, value ) (Internal.Field.Field field kind) =
    Internal.Field.Field
        { field | properties = ( key, Encode.string value ) :: field.properties }
        kind


{-| -}
withInitialValue : (input -> initial) -> Field error value input initial kind msg constraints -> Field error value input initial kind msg constraints
withInitialValue toInitialValue (Internal.Field.Field field kind) =
    Internal.Field.Field
        { field | initialValue = toInitialValue >> field.initialToString >> Just }
        kind


{-| -}
withOptionalInitialValue : (input -> Maybe initial) -> Field error value input initial kind msg constraints -> Field error value input initial kind msg constraints
withOptionalInitialValue toInitialValue (Internal.Field.Field field kind) =
    Internal.Field.Field
        { field
            | initialValue =
                toInitialValue >> Maybe.map field.initialToString
        }
        kind


{-| -}
withOnChange : (Result String initial -> msg) -> Field error value input initial kind msg constraints -> Field error value input initial kind msg constraints
withOnChange toMsg (Internal.Field.Field field kind) =
    Internal.Field.Field
        { field
            | toMsg =
                Just
                    (\{ event, thisField } ->
                        case event.event of
                            InputEvent newValue ->
                                if event.name == thisField then
                                    let
                                        foo : Maybe initial
                                        foo =
                                            field.initialFromString newValue
                                    in
                                    foo
                                        |> Maybe.map (toMsg << Ok)
                                        |> Maybe.withDefault
                                            (toMsg (Err ""))

                                else
                                    toMsg (Err "")

                            _ ->
                                toMsg (Err "")
                    )
        }
        kind

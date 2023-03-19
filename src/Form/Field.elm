module Form.Field exposing
    ( text, checkbox, int, float
    , select, OutsideRange(..)
    , date, time, TimeOfDay
    , Field(..), FieldInfo, exactValue
    , required, withClientValidation, map
    , email, password, search, telephone, url, textarea
    , withMinLength, withMaxLength
    , No, Yes
    --, withMax, withStep, withMin, range,
    --withInitialValue, withOptionalInitialValue
    )

{-|


## Base Fields

@docs text, checkbox, int, float


## Multiple Choice Fields

@docs select, OutsideRange


## Date/Time Fields

@docs date, time, TimeOfDay


## Other

@docs Field, FieldInfo, exactValue


## Field Configuration

@docs required, withClientValidation, map


## Text Field Display Options

@docs email, password, search, telephone, url, textarea


## Numeric Field Options

@docs withMinLength, withMaxLength


## Phantom Options

@docs No, Yes

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Form.FieldView exposing (Input)
import Internal.Input exposing (Options(..))
import Json.Encode as Encode


{-| -}
type Field error parsed data initial kind constraints
    = Field (FieldInfo error parsed data initial) kind


{-| -}
type alias FieldInfo error parsed data initial =
    { initialValue : Maybe (data -> Maybe String)
    , decode : Maybe String -> ( Maybe parsed, List error )
    , properties : List ( String, Encode.Value )
    , initialToString : initial -> String
    }


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
            data
            kind
            initial
            { constraints
                | required : ()
                , wasMapped : No
            }
    -> Field error parsed data kind initial { constraints | wasMapped : No }
required missingError (Field field kind) =
    Field
        { initialValue = field.initialValue
        , initialToString = field.initialToString
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
        }
        kind


{-| -}
text :
    Field
        error
        (Maybe String)
        data
        String
        Input
        { required : ()
        , plainText : ()
        , wasMapped : No
        , minlength : ()
        , maxlength : ()
        }
text =
    Field
        { initialValue = Nothing
        , initialToString = identity
        , decode =
            \rawValue ->
                ( if rawValue == Just "" then
                    Just Nothing

                  else
                    Just rawValue
                , []
                )
        , properties = []
        }
        (Internal.Input.Input Internal.Input.Text)


{-| -}
date :
    { invalid : String -> error }
    ->
        Field
            error
            (Maybe Date)
            data
            Date
            Input
            { min : Date
            , max : Date
            , required : ()
            , wasMapped : No
            }
date toError =
    Field
        { initialValue = Nothing
        , initialToString = Date.toIsoString
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
            data
            TimeOfDay
            Input
            { -- TODO support min/max
              --min : ???,
              --, max : ???,
              required : ()
            , wasMapped : No
            }
time toError =
    Field
        { initialValue = Nothing
        , initialToString = timeOfDayToString
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
            data
            option
            (Options option)
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
    Field
        { initialValue = Nothing
        , initialToString = enumToString optionsMapping
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
            data
            Never
            Input
            { required : ()
            , plainText : ()
            , wasMapped : No
            }
exactValue initialValue error =
    Field
        { initialValue = Just (\_ -> Just initialValue)
        , initialToString = never
        , decode =
            \rawValue ->
                if rawValue == Just initialValue then
                    ( rawValue, [] )

                else
                    ( rawValue, [ error ] )
        , properties = []
        }
        (Internal.Input.Input Internal.Input.Text)


{-| -}
checkbox :
    Field
        error
        Bool
        data
        Bool
        Input
        { required : ()
        }
checkbox =
    Field
        { initialValue = Nothing
        , initialToString =
            \bool ->
                if bool then
                    "on"

                else
                    ""
        , decode =
            \rawString ->
                ( (rawString == Just "on")
                    |> Just
                , []
                )
        , properties = []
        }
        (Internal.Input.Input Internal.Input.Checkbox)


{-| -}
int :
    { invalid : String -> error }
    ->
        Field
            error
            (Maybe Int)
            data
            Int
            Input
            { min : Int
            , max : Int
            , required : ()
            , wasMapped : No
            , step : Int
            }
int toError =
    Field
        { initialValue = Nothing
        , initialToString = String.fromInt
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
        }
        (Internal.Input.Input Internal.Input.Number)


{-| -}
float :
    { invalid : String -> error }
    ->
        Field
            error
            (Maybe Float)
            data
            Float
            Input
            { min : Float
            , max : Float
            , required : ()
            , wasMapped : No
            }
float toError =
    Field
        { initialValue = Nothing
        , initialToString = String.fromFloat
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
        }
        (Internal.Input.Input Internal.Input.Number)


{-| -}
telephone :
    Field error parsed data initial Input { constraints | plainText : () }
    -> Field error parsed data initial Input constraints
telephone (Field field _) =
    Field field
        (Internal.Input.Input Internal.Input.Tel)


{-| -}
search :
    Field error parsed data initial Input { constraints | plainText : () }
    -> Field error parsed data initial Input constraints
search (Field field _) =
    Field field
        (Internal.Input.Input Internal.Input.Search)


{-| -}
password :
    Field error parsed data initial Input { constraints | plainText : () }
    -> Field error parsed data initial Input constraints
password (Field field _) =
    Field field
        (Internal.Input.Input Internal.Input.Password)


{-| -}
email :
    Field error parsed data initial Input { constraints | plainText : () }
    -> Field error parsed data initial Input constraints
email (Field field _) =
    Field field
        (Internal.Input.Input Internal.Input.Email)


{-| -}
url :
    Field error parsed data initial Input { constraints | plainText : () }
    -> Field error parsed data initial Input constraints
url (Field field _) =
    Field field
        (Internal.Input.Input Internal.Input.Url)


{-| -}
textarea :
    { rows : Maybe Int, cols : Maybe Int }
    -> Field error parsed data initial Input { constraints | plainText : () }
    -> Field error parsed data initial Input constraints
textarea options (Field field _) =
    Field field (Internal.Input.Input (Internal.Input.Textarea options))


{-| -}
type OutsideRange
    = AboveRange
    | BelowRange



--{-| -}
--range :
--    { min : Form.Value.Value valueType
--    , max : Form.Value.Value valueType
--    , initial : data -> Form.Value.Value valueType
--    , missing : error
--    , invalid : OutsideRange -> error
--    }
--    ->
--        Field
--            error
--            (Maybe valueType)
--            data
--            Never
--            -- TODO
--            kind
--            { constraints
--                | required : ()
--                , initial : valueType
--                , min : valueType
--                , max : valueType
--                , wasMapped : No
--            }
--    ->
--        Field
--            error
--            valueType
--            data
--            Never
--            -- TODO
--            Input
--            { constraints | wasMapped : No }
--range info field =
--    field
--        |> required info.missing
--        |> withMin info.min (info.invalid BelowRange)
--        |> withMax info.max (info.invalid AboveRange)
--        |> (\(Field innerField _) -> Field { innerField | initialValue = Just (info.initial >> Form.Value.toString >> Just) } (Internal.Input.Input Internal.Input.Range))


{-| -}
map : (parsed -> mapped) -> Field error parsed data initial kind constraints -> Field error mapped data initial kind { constraints | wasMapped : Yes }
map mapFn field_ =
    withClientValidation
        (\value -> ( Just (mapFn value), [] ))
        field_


{-| -}
withClientValidation : (parsed -> ( Maybe mapped, List error )) -> Field error parsed data initial kind constraints -> Field error mapped data initial kind { constraints | wasMapped : Yes }
withClientValidation mapFn (Field field kind) =
    Field
        { initialValue = field.initialValue
        , initialToString = field.initialToString
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
        }
        kind



--{-| -}
--withMin : Form.Value.Value valueType -> error -> Field error parsed data initial kind { constraints | min : valueType } -> Field error parsed data initial kind constraints
--withMin min error (Field field kind) =
--    Field
--        { initialValue = field.initialValue
--        , initialToString = field.initialToString
--        , decode =
--            \value ->
--                value
--                    |> field.decode
--                    |> (\( maybeValue, errors ) ->
--                            case maybeValue of
--                                Nothing ->
--                                    ( Nothing, errors )
--
--                                Just okValue ->
--                                    if isEmptyValue value then
--                                        ( Just okValue, errors )
--
--                                    else
--                                        case Form.Value.compare (value |> Maybe.withDefault "") min of
--                                            LT ->
--                                                ( Just okValue, error :: errors )
--
--                                            _ ->
--                                                ( Just okValue, errors )
--                       )
--        , properties = ( "min", Encode.string (Form.Value.toString min) ) :: field.properties
--        }
--        kind


{-| -}
withMinLength : Int -> error -> Field error parsed data initial kind { constraints | minlength : () } -> Field error parsed data initial kind constraints
withMinLength minLength error (Field field kind) =
    Field
        { initialValue = field.initialValue
        , initialToString = field.initialToString
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
        }
        kind


{-| -}
withMaxLength : Int -> error -> Field error parsed data initial kind { constraints | maxlength : () } -> Field error parsed data initial kind constraints
withMaxLength maxLength error (Field field kind) =
    Field
        { initialValue = field.initialValue
        , initialToString = field.initialToString
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
        }
        kind


isEmptyValue : Maybe String -> Bool
isEmptyValue value =
    (value |> Maybe.withDefault "") == ""



--{-| -}
--withMax : Form.Value.Value valueType -> error -> Field error parsed data initial kind { constraints | max : valueType } -> Field error parsed data initial kind constraints
--withMax max error (Field field kind) =
--    Field
--        { initialValue = field.initialValue
--        , initialToString = field.initialToString
--        , decode =
--            \value ->
--                value
--                    |> field.decode
--                    |> (\( maybeValue, errors ) ->
--                            case maybeValue of
--                                Nothing ->
--                                    ( Nothing, errors )
--
--                                Just okValue ->
--                                    if isEmptyValue value then
--                                        ( Just okValue, errors )
--
--                                    else
--                                        case Form.Value.compare (value |> Maybe.withDefault "") max of
--                                            GT ->
--                                                ( Just okValue, error :: errors )
--
--                                            _ ->
--                                                ( Just okValue, errors )
--                       )
--        , properties = ( "max", Encode.string (Form.Value.toString max) ) :: field.properties
--        }
--        kind
--
--
--{-| -}
--withStep : Form.Value.Value valueType -> Field msg error value initial view { constraints | step : valueType } -> Field msg error value initial view constraints
--withStep max field =
--    withStringProperty ( "step", Form.Value.toString max ) field


withStringProperty : ( String, String ) -> Field error parsed data initial kind constraints1 -> Field error parsed data initial kind constraints2
withStringProperty ( key, value ) (Field field kind) =
    Field
        { field | properties = ( key, Encode.string value ) :: field.properties }
        kind

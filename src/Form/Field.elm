module Form.Field exposing
    ( Field
    , text, checkbox, int, float
    , select, OutsideRange(..)
    , date, time, TimeOfDay
    , withInitialValue, withOptionalInitialValue
    , exactValue
    , required, validateMap, map
    , email, password, search, telephone, url, textarea
    , range, withMin, withMax
    , withMinLength, withMaxLength
    , withStep
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

@docs required, validateMap, map


## Text Field Display Options

@docs email, password, search, telephone, url, textarea


## Numeric Field Options

@docs range, withMin, withMax

@docs withMinLength, withMaxLength

@docs withStep


## Phantom Options

@docs No, Yes

-}

import Date exposing (Date)
import Dict exposing (Dict)
import Form.FieldView exposing (Input)
import Internal.Field
import Internal.Input exposing (Options(..))
import Json.Encode as Encode


{-| A `Field` represents the base information of how to turn a raw input into a parsed value, and how to display that value
as an HTML form field element. Note that you can also perform more advanced validations and mapping using the
[`Form.Validation`](Form-Validation) API in your `combine` function.

For example, if you want to display a check-in and check-out date field, you would use `date` Fields. Using [`date`](#date)
does two things:

1.  Sets display options for the browser to display the field with [`<input type="date">`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/date).

2.  Parses into an Elm value of type [`Date`](https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#Date) (or a validation error if the format isn't invalid).

Why would the date format be invalid if it is managed by the Browser's date picker element? In the happy path on the Browser this will never happen.

However, you can't make any assumptions about the client (non-standard clients could be used), or about the data format that is sent to servers.
Often validation logic is duplicated to guard against this on both the client (display useful validation feedback for the user) and the server (validate untrusted input).
If you use full-stack Elm, you can use your Form definition on your server to run the same code that
you use to present validation errors on the client, allowing you to keep the validations in sync.

Here is an example that showcases different types of Fields (`text`, `date`, `time`, and `checkbox`), as well as how to use the `combine` function to
transform the parsed values and perform additional validations between the parsed fields.

    import Date exposing (Date)
    import Form
    import Form.Field as Field
    import Form.Validation as Validation

    type alias Stay =
        { name : String
        , checkIn : Checkin
        , emailUpdates : Bool
        }

    type alias Checkin =
        { date : Date
        , nights : Int
        , time : TimeOfDay
        }

    example : Form.HtmlForm String Stay input Msg
    example =
        (\name checkIn checkOut checkInTime emailUpdates ->
            { combine =
                Validation.succeed Stay
                    |> Validation.andMap name
                    |> Validation.andMap
                        (Validation.succeed
                            (\checkinValue checkoutValue checkInTimeValue ->
                                Validation.succeed
                                    { date = checkinValue
                                    , nights = Date.toRataDie checkoutValue - Date.toRataDie checkinValue
                                    , time = checkInTimeValue
                                    }
                                    |> Validation.withErrorIf (Date.toRataDie checkinValue >= Date.toRataDie checkoutValue) checkIn "Must be before checkout"
                            )
                            |> Validation.andMap checkIn
                            |> Validation.andMap checkOut
                            |> Validation.andMap checkInTime
                            |> Validation.andThen identity
                        )
                    |> Validation.andMap emailUpdates
            , view =
                \formState ->
                    let
                        fieldView label field =
                            Html.div []
                                [ Html.label []
                                    [ Html.text (label ++ " ")
                                    , FieldView.input [] field
                                    , errorsView formState field
                                    ]
                                ]
                    in
                    [ fieldView "Name" name
                    , fieldView "Check-In" checkIn
                    , fieldView "Check-Out" checkOut
                    , fieldView "Check-In Time" checkInTime
                    , fieldView "Sign Up For Email Updates" emailUpdates
                    , Html.button [] [ Html.text "Submit" ]
                    ]
            }
        )
            |> Form.form
            |> Form.field "name"
                (Field.text
                    |> Field.required "Required"
                )
            |> Form.field "checkin"
                (Field.date
                    { invalid = \_ -> "Invalid" }
                    |> Field.required "Required"
                    |> Field.withMin today ("Must be after " ++ Date.toIsoString today)
                )
            |> Form.field "checkout"
                (Field.date
                    { invalid = \_ -> "Invalid" }
                    |> Field.required "Required"
                )
            |> Form.field "checkinTime"
                (Field.time
                    { invalid = \_ -> "Invalid" }
                    |> Field.required "Required"
                    |> Field.withMin { hours = 10, minutes = 0 } "Must be after today"
                )
            |> Form.field "emailUpdates"
                Field.checkbox

    today : Date
    today =
        Date.fromRataDie 738624

The key concepts to understand here are where the view and validation logic for fields lives.

The `Form`'s `view` function is responsible for combining the rendered Fields, but the `Field` contains the information for how to display
the form field itself (details like like `<input type="date">`, `<input type="checkbox">`, `<textarea>` etc.). Since form fields contain both
display logic that changes how we parse/validate the field, all of the parsing and validation logic related to displaying the
Field is also defined with `Field` type. For example, `Field.withMin` contains information that is used both for displaying the form field and for
validating it. When we set `Field.withMin`, it gives the Browser information on how to restrict the date picker UI from showing
invalid dates, but setting that minimum value also runs that validation when we run the Form parser, even if we run it
on a server through code sharing.

Note that the validations in a `Field`s definition (such as `Field.withMin`, or `Field.date`, etc.) are run
regardless of whether you use that field in the Form's `combine` function.

-}
type alias Field error parsed input initial kind constraints =
    Internal.Field.Field error parsed input initial kind constraints


{-| Used in the constraints for a Field. These can't be built or used outside of the API, they are only used as guardrails
to ensure sure that Fields are configured correctly.
-}
type Yes
    = Yes Never


{-| Used in the constraints for a Field. These can't be built or used outside of the API, they are only used as guardrails

to ensure sure that Fields are configured correctly.

-}
type No
    = No Never


{-| Gives a validation error for fields that haven't been set, and removes the `Maybe` around the parsed value.

    example =
        Field.int { invalid = \_ -> "Invalid" }
            -- parses into `Maybe Int` before we call `required`
            -- after `required`, it parses into an `Int`
            |> Field.required "Required"

-}
required :
    error
    ->
        Field
            error
            (Maybe parsed)
            kind
            input
            initial
            { constraints
                | required : ()
                , wasMapped : No
            }
    -> Field error parsed kind input initial { constraints | wasMapped : No }
required missingError (Internal.Field.Field field kind) =
    Internal.Field.Field
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
        , properties =
            ( "required", Encode.bool True ) :: field.properties
        , compare = field.compare
        }
        kind


{-| The base for a text field. You can add display modifiers to text fields, including displaying them as a `textarea`.
See [Text Field Display Options](#text-field-display-options).

By default, text fields are not required. If the field is not touched or has been deleted, the value will be `Nothing`
(_not_ empty string `Just ""`). See [`required`](#required).

    import Form.Field as Field

    type alias Profile =
        { status : Maybe String
        }

    example =
        (\username ->
            { combine =
                Validation.succeed Status
                    |> Validation.andMap username
            , view = []
            }
        )
            |> Form.form
            |> Form.field "status" Field.text

-}
text :
    Field
        error
        (Maybe String)
        input
        String
        Input
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
        }
        (Internal.Input.Input Internal.Input.Text)


{-| A date field. Parses into a value of type [`Date`](https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#Date).

    example =
        Field.date
            { invalid = \_ -> "Invalid date" }
            |> Field.required "Required"
            |> Field.withMin (Date.fromRataDie 738624) "Must be after today"
            -- date picker will show dates on the same day of the week starting from the start date
            |> Field.withStep 7

-}
date :
    { invalid : String -> error }
    ->
        Field
            error
            (Maybe Date)
            input
            Date
            Input
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
        }
        (Internal.Input.Input Internal.Input.Date)


{-| A time of day in 24-hour time.

The hours must be between 0 and 23, and the minutes must be between 0 and 59.

This is the type that a `time` field parses into, and is also used to set initial values and minimum/maximum values for `time`.

See <https://developer.mozilla.org/en-US/docs/Web/HTML/Date_and_time_formats#time_strings>

-}
type alias TimeOfDay =
    { hours : Int
    , minutes : Int
    , seconds : Maybe Int
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
            { min : TimeOfDay
            , max : TimeOfDay
            , required : ()
            , wasMapped : No
            , step : Int
            }
time toError =
    Internal.Field.Field
        { initialValue = \_ -> Nothing
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
        , compare =
            \raw value ->
                parseTimeOfDay raw
                    |> Result.map
                        (\parsedRaw ->
                            if parsedRaw.hours == value.hours then
                                if parsedRaw.minutes == value.minutes then
                                    Basics.compare
                                        (value.seconds |> Maybe.withDefault 0)
                                        (parsedRaw.seconds |> Maybe.withDefault 0)

                                else
                                    Basics.compare value.minutes parsedRaw.minutes

                            else
                                Basics.compare value.hours parsedRaw.hours
                        )
                    |> Result.withDefault LT
        }
        (Internal.Input.Input Internal.Input.Time)


timeOfDayToString : TimeOfDay -> String
timeOfDayToString { hours, minutes, seconds } =
    let
        secondsPart : String
        secondsPart =
            case seconds of
                Just justSeconds ->
                    ":" ++ paddedInt justSeconds

                Nothing ->
                    ""
    in
    paddedInt hours ++ ":" ++ paddedInt minutes ++ secondsPart


paddedInt : Int -> String
paddedInt intValue =
    intValue
        |> String.fromInt
        |> String.padLeft 2 '0'


parseTimeOfDay : String -> Result () TimeOfDay
parseTimeOfDay rawTimeOfDay =
    case rawTimeOfDay |> String.split ":" |> List.map String.toInt of
        [ Just hours, Just minutes, Just seconds ] ->
            Ok
                { hours = hours
                , minutes = minutes
                , seconds = Just seconds
                }

        [ Just hours, Just minutes ] ->
            Ok
                { hours = hours
                , minutes = minutes
                , seconds = Nothing
                }

        _ ->
            Err ()


{-| An input for a set of possible options. Can be rendered in two ways

  - As a dropdown (`<select>`)[`Form.FieldView.select`](Form-FieldView#select)
  - As a set of radio buttons (`<input type="radio">`)[`Form.FieldView.radio`](Form-FieldView#radio).

```elm
import Form
import Form.Field as Field
import Form.FieldView as FieldView
import Form.Validation as Validation

sizeForm : Form.HtmlForm String Size input msg
sizeForm =
    (\size ->
        { combine =
            Validation.succeed identity
                |> Validation.andMap size
        , view =
            \formState ->
                [ Html.div []
                    [ FieldView.select []
                        (\entry -> ( [], sizeToString entry ))
                        size
                    ]
                , Html.button [] [ Html.text "Submit" ]
                ]
        }
    )
        |> Form.form
        |> Form.field "size"
            (Field.select
                [ ( "small", Small )
                , ( "medium", Medium )
                , ( "large", Large )
                ]
                (\_ -> "Invalid")
                |> Field.required "Required"
                |> Field.withInitialValue (\_ -> Small)
            )

sizeToString : Size -> String
sizeToString size =
    case size of
        Small ->
            "Small"

        Medium ->
            "Medium"

        Large ->
            "Large"

type Size
    = Small
    | Medium
    | Large
```

-}
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
        }
        (Options fromString (optionsMapping |> List.map Tuple.first))


enumToString : List ( String, enum ) -> enum -> String
enumToString optionsMapping a =
    case optionsMapping |> List.filter (\( _, b ) -> b == a) |> List.head of
        Just ( str, _ ) ->
            str

        Nothing ->
            "Missing enum"


{-| Render a field with a hardcoded value.
-}
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
            { required : ()
            , plainText : ()
            , wasMapped : No
            }
exactValue initialValue error =
    Internal.Field.Field
        { initialValue = \_ -> Nothing
        , initialToString = never
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
        }
        (Internal.Input.Input Internal.Input.Text)


{-| Renders a checkbox input (`<input type="checkbox">`), see <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/checkbox>.

    import Form.Field as Field

    example =
        Field.checkbox

-}
checkbox :
    Field
        error
        Bool
        input
        Bool
        Input
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
        }
        (Internal.Input.Input Internal.Input.Checkbox)


{-| Renders a number input (`<input type="number">`), see <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/number>.

Floating point numbers will give a validation error, using the error value passed in through the `invalid` function.

    import Form.Field as Field

    example =
        Field.int
            { invalid =
                \value -> "Must be an integer"
            }

-}
int :
    { invalid : String -> error }
    ->
        Field
            error
            (Maybe Int)
            input
            Int
            Input
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
                        Basics.compare value parsed

                    _ ->
                        LT
        }
        (Internal.Input.Input Internal.Input.Number)


{-| A number input (`<input type="number">`), see <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/number>.

Unlike [`int`](#int), this field allows floating point numbers.
It will give a validation error if the input is not a number, using the error value passed in through the `invalid` function.

    import Form.Field as Field

    example =
        Field.float
            { invalid =
                \value -> "Must be a number"
            }

-}
float :
    { invalid : String -> error }
    ->
        Field
            error
            (Maybe Float)
            input
            Float
            Input
            { min : Float
            , max : Float
            , required : ()
            , wasMapped : No
            }
float toError =
    Internal.Field.Field
        { initialValue = \_ -> Nothing
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
        , compare =
            \raw value ->
                case String.toFloat raw of
                    Just parsed ->
                        Basics.compare value parsed

                    _ ->
                        LT
        }
        (Internal.Input.Input Internal.Input.Number)


{-| Modifier for [`text`](#text) Field. This is only a display hint to the browser (`<input type="tel">`).

This is especially important on mobile devices for ensuring that the correct keyboard is displayed for inputting a phone number.

See <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/tel>.

    example =
        Field.text
            |> Field.telephone

-}
telephone :
    Field error parsed input initial Input { constraints | plainText : () }
    -> Field error parsed input initial Input constraints
telephone (Internal.Field.Field field _) =
    Internal.Field.Field field
        (Internal.Input.Input Internal.Input.Tel)


{-| Modifier for [`text`](#text) Field. This changes the display of the Field to a password input (`<input type="search">`).
On mobile devices, this will display a keyboard with a search button.

See <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/search>.

    example =
        Field.text
            |> Field.search

-}
search :
    Field error parsed input initial Input { constraints | plainText : () }
    -> Field error parsed input initial Input constraints
search (Internal.Field.Field field _) =
    Internal.Field.Field field
        (Internal.Input.Input Internal.Input.Search)


{-| Modifier for [`text`](#text) Field. This is only a display hint to the browser that the Field should be displayed as a password input (`<input type="password">`).

See <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/password>.

    example =
        Field.text
            |> Field.password
            |> Field.required "Password is required"

-}
password :
    Field error parsed input initial Input { constraints | plainText : () }
    -> Field error parsed input initial Input constraints
password (Internal.Field.Field field _) =
    Internal.Field.Field field
        (Internal.Input.Input Internal.Input.Password)


{-| Modifier for [`text`](#text) Field. This does not perform any additional validations on the Field, it only provides a hint to the browser
that the Field should be displayed as an email input (`<input type="email">`). This is especially useful for mobile devices to make sure
the correct keyboard is displayed.

See <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/email>.

    example =
        Field.text
            |> Field.email
            |> Field.required "Email is required"

-}
email :
    Field error parsed input initial Input { constraints | plainText : () }
    -> Field error parsed input initial Input constraints
email (Internal.Field.Field field _) =
    Internal.Field.Field field
        (Internal.Input.Input Internal.Input.Email)


{-| Modifier for [`text`](#text) Field. This does not perform any additional validations on the Field, it only provides a hint to the browser
that the Field should be displayed as a URL input (`<input type="url">`).

See <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/url>.

    example =
        Field.text
            |> Field.url
            |> Field.required "URL is required"

-}
url :
    Field error parsed input initial Input { constraints | plainText : () }
    -> Field error parsed input initial Input constraints
url (Internal.Field.Field field _) =
    Internal.Field.Field field
        (Internal.Input.Input Internal.Input.Url)


{-| Modifier for [`text`](#text) Field to display it as a [`textarea`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea).

`textarea` are for multi-line text input. For example, you might use a regular `text` Field for a username, and a `textarea` Field for a biography.

    import Form.Field as Field

    type alias Profile =
        { username : String
        , bio : String
        }

    example =
        (\username bio ->
            { combine =
                Validation.succeed Profile
                    |> Validation.andMap username
                    |> Validation.andMap bio
            , view = []
            }
        )
            |> Form.form
            |> Form.field "username"
                (Field.text
                    |> Field.required "Required"
                )
            |> Form.field "bio"
                (Field.text
                    |> Field.textarea
                        { rows = Just 20
                        , cols = Just 50
                        }
                    |> Field.required "Required"
                )

-}
textarea :
    { rows : Maybe Int, cols : Maybe Int }
    -> Field error parsed input initial Input { constraints | plainText : () }
    -> Field error parsed input initial Input constraints
textarea options (Internal.Field.Field field _) =
    Internal.Field.Field field (Internal.Input.Input (Internal.Input.Textarea options))


{-| Used for errors from a [`range`](#range) Field.
-}
type OutsideRange
    = AboveRange
    | BelowRange


{-| Display a range input (`<input type="range">`). See <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/range>.

    import Form.Field as Field

    type alias Settings =
        { brightness : Int
        }

    example =
        (\brightness ->
            { combine =
                Validation.succeed Settings
                    |> Validation.andMap brightness
            , view = []
            }
        )
            |> Form.form
            |> Form.field "brightness"
                (Field.range
                    { min = 0
                    , max = 100
                    , missing = "Required"
                    , invalid =
                        \outsideRange ->
                            case outsideRange of
                                Field.AboveRange ->
                                    "Must be below 100"

                                Field.BelowRange ->
                                    "Must be above 0"
                    }
                    (Field.int { invalid = \_ -> "Invalid" })
                )

Can be used with either [`int`](#int) or [`float`](#float).

-}
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
            { constraints | wasMapped : No }
range info field =
    -- TODO set the default value (if not overridden) using this https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/range#value
    field
        |> required info.missing
        |> withMin info.min (info.invalid BelowRange)
        |> withMax info.max (info.invalid AboveRange)
        |> (\(Internal.Field.Field innerField _) -> Internal.Field.Field innerField (Internal.Input.Input Internal.Input.Range))


{-| Map the parsed value of a Field without adding or modifying its validations or rendering.

    import Form.Field as Field

    example =
        Field.text
            |> Field.required "Required"
            |> Field.map String.toUpper

-}
map : (parsed -> mapped) -> Field error parsed input initial kind constraints -> Field error mapped input initial kind { constraints | wasMapped : Yes }
map mapFn field_ =
    validateMap
        (\value -> Ok (mapFn value))
        field_


{-| Add a custom validation and/or transformation of the value to the field.

    import Form.Field as Field

    example =
        Field.text
            |> Field.required "Required"
            |> Field.validateMap Username.fromString

      -- in Username.elm
      fromString : String -> Result String Username
      fromString string =
          if string |> String.contains "@" then
              Err "Must not contain @"

          else
              Username string |> Ok

-}
validateMap : (parsed -> Result error mapped) -> Field error parsed input initial kind constraints -> Field error mapped input initial kind { constraints | wasMapped : Yes }
validateMap mapFn (Internal.Field.Field field kind) =
    validateMap_
        (\parsed ->
            case mapFn parsed of
                Ok mapped ->
                    ( Just mapped, [] )

                Err error ->
                    ( Nothing, [ error ] )
        )
        (Internal.Field.Field field kind)


validateMap_ : (parsed -> ( Maybe mapped, List error )) -> Field error parsed input initial kind constraints -> Field error mapped input initial kind { constraints | wasMapped : Yes }
validateMap_ mapFn (Internal.Field.Field field kind) =
    Internal.Field.Field
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
        , compare = field.compare
        }
        kind


{-| Set the min value for the Field. This results in both a validation (run on the server as well as the client) as well as a display hint to the browser
(`<input type="date" min="2023-04-14">`). The Browser will prevent the user from entering a value below the min value in some cases but not all.

See <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/min>.

If the value is invalid (below the minimum), the error will be whichever error is passed in as the second argument.

    import Date exposing (Date)
    import Form.Field as Field

    example =
        Field.date
            { invalid = \_ -> "Must be valid date" }
            |> Field.required "Required"
            |> Field.withMin today ("Must be after " ++ Date.toIsoString today)

    today : Date
    today =
        Date.fromRataDie 738624

-}
withMin : initial -> error -> Field error parsed input initial kind { constraints | min : initial } -> Field error parsed input initial kind constraints
withMin min error (Internal.Field.Field field kind) =
    Internal.Field.Field
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
                                    if isEmptyValue value then
                                        ( Just okValue, errors )

                                    else
                                        case field.compare (value |> Maybe.withDefault "") min of
                                            GT ->
                                                ( Just okValue, error :: errors )

                                            _ ->
                                                ( Just okValue, errors )
                       )
        , properties = ( "min", Encode.string (field.initialToString min) ) :: field.properties
        , compare = field.compare
        }
        kind


{-| Set a minimum length for the string. See <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/minlength>.
-}
withMinLength : Int -> error -> Field error parsed input initial kind { constraints | minlength : () } -> Field error parsed input initial kind constraints
withMinLength minLength error (Internal.Field.Field field kind) =
    Internal.Field.Field
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
        , compare = field.compare
        }
        kind


{-| Set a maximum length for the string. See <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/maxlength>.
-}
withMaxLength : Int -> error -> Field error parsed input initial kind { constraints | maxlength : () } -> Field error parsed input initial kind constraints
withMaxLength maxLength error (Internal.Field.Field field kind) =
    Internal.Field.Field
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
        , compare = field.compare
        }
        kind


isEmptyValue : Maybe String -> Bool
isEmptyValue value =
    (value |> Maybe.withDefault "") == ""


{-| Same as [`withMin`](#withMin) but for a maximum value. See <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/max>.

    import Date exposing (Date)
    import Form.Field as Field

    example =
        Field.date
            { invalid = \_ -> "Must be valid date" }
            |> Field.required "Required"
            |> Field.withMax today "Cannot schedule more than 7 days in advance"

    inAWeek : Date
    inAWeek =
        Date.fromRataDie (today + 7)

    today : Int
    today =
        738624

-}
withMax : initial -> error -> Field error parsed input initial kind { constraints | max : initial } -> Field error parsed input initial kind constraints
withMax max error (Internal.Field.Field field kind) =
    Internal.Field.Field
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
                                    if isEmptyValue value then
                                        ( Just okValue, errors )

                                    else
                                        case field.compare (value |> Maybe.withDefault "") max of
                                            LT ->
                                                ( Just okValue, error :: errors )

                                            _ ->
                                                ( Just okValue, errors )
                       )
        , properties = ( "max", Encode.string (field.initialToString max) ) :: field.properties
        , compare = field.compare
        }
        kind


{-| Sets the `step` attribute on the form field for Field's with `Int` steps. See <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/step>.

For `int` fields, the step will change the up/down buttons in the field's UI to increment/decrement by the given step.

`Int` step values have different meanings for different kinds of Fields.

  - [`date`](#date) - number of days
  - [`time`](#time) - time in seconds

-}
withStep : Int -> Field error value input initial view { constraints | step : Int } -> Field error value input initial view constraints
withStep step (Internal.Field.Field info kind) =
    withStringProperty ( "step", String.fromInt step ) (Internal.Field.Field info kind)


withStringProperty : ( String, String ) -> Field error parsed input initial kind constraints1 -> Field error parsed input initial kind constraints2
withStringProperty ( key, value ) (Internal.Field.Field field kind) =
    Internal.Field.Field
        { field | properties = ( key, Encode.string value ) :: field.properties }
        kind


{-| Set an initial value for the `Field` given the `Form`'s `input` (see [`Form.withInput`](Form#withInput)).
This allows you to pass in dynamic state like values from your `Model`.

The initial value will be used until the field is modified by the user, and from there it is controlled by user input. If you
need to programmatically set a field's value for more advanced use cases, you can also modify the [`Form.Model`](Form#Model).

The type you use to set the initial value depends on the Field. For example, you can set a `checkbox` Field's initial value
with a `Bool`

    example =
        Form.checkbox |> Form.withInitialValue .autoplay

    formOptions : { autoplay : Bool } -> Form.Options String parsed { autoplay : Bool } msg
    formOptions currentSettings =
        Form.options "settings"
            |> Form.withInput currentSettings

Note that the type used to set the initial value is independent of types you might `map` a field into.

-}
withInitialValue : (input -> initial) -> Field error value input initial kind constraints -> Field error value input initial kind constraints
withInitialValue toInitialValue (Internal.Field.Field field kind) =
    Internal.Field.Field
        { field | initialValue = toInitialValue >> field.initialToString >> Just }
        kind


{-| Similar to [`withInitialValue`](#withInitialValue), but takes in a `Maybe` value. If the `Maybe` is `Nothing` then it's
the same as if no initial value were set.

    example =
        Form.text |> Form.withOptionalInitialValue .nickname

    formOptions : { nickname : Maybe String } -> Form.Options String parsed { nickname : Maybe String } msg
    formOptions currentProfile =
        Form.options "profile"
            |> Form.withInput currentProfile

-}
withOptionalInitialValue : (input -> Maybe initial) -> Field error value input initial kind constraints -> Field error value input initial kind constraints
withOptionalInitialValue toInitialValue (Internal.Field.Field field kind) =
    Internal.Field.Field
        { field
            | initialValue =
                toInitialValue >> Maybe.map field.initialToString
        }
        kind

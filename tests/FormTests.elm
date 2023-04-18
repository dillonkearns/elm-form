module FormTests exposing (all)

import Date exposing (Date)
import Dict
import Expect
import Form exposing (Validated(..))
import Form.Field as Field
import Form.Handler
import Form.Validation as Validation
import Test exposing (Test, describe, test)


type Uuid
    = Uuid String


type Action
    = Signout
    | SetQuantity ( Uuid, Int )


type alias DoneForm error parsed input view =
    Form.Form
        error
        { combine : Validation.Validation error parsed Never Never
        , view : Form.Context error input -> view
        }
        parsed
        input


all : Test
all =
    describe "Form Parser" <|
        let
            passwordConfirmationParser =
                Form.form
                    (\password passwordConfirmation ->
                        { combine =
                            password
                                |> Validation.andThen
                                    (\passwordValue ->
                                        passwordConfirmation
                                            |> Validation.map
                                                (\passwordConfirmationValue ->
                                                    if passwordValue == passwordConfirmationValue then
                                                        Ok { password = passwordValue }

                                                    else
                                                        Err "Must match password"
                                                )
                                            |> Validation.fromResult
                                    )
                        , view = \_ -> Div
                        }
                    )
                    |> Form.field "password" (Field.text |> Field.required "Password is required")
                    |> Form.field "password-confirmation" (Field.text |> Field.required "Password confirmation is required")
        in
        [ test "matching password" <|
            \() ->
                Form.Handler.run
                    (fields
                        [ ( "password", "mypassword" )
                        , ( "password-confirmation", "mypassword" )
                        ]
                    )
                    (passwordConfirmationParser |> Form.Handler.init identity)
                    |> Expect.equal
                        (Valid { password = "mypassword" })
        , test "non-matching password" <|
            \() ->
                Form.Handler.run
                    (fields
                        [ ( "password", "mypassword" )
                        , ( "password-confirmation", "doesnt-match-password" )
                        ]
                    )
                    (passwordConfirmationParser |> Form.Handler.init identity)
                    |> Expect.equal
                        (Invalid Nothing
                            (Dict.fromList [ ( "password-confirmation", [ "Must match password" ] ) ])
                        )
        , describe "oneOf" <|
            let
                oneOfParsers : Form.Handler.Handler String Action
                oneOfParsers =
                    Form.Handler.init SetQuantity
                        (Form.form
                            (\_ uuid quantity ->
                                { combine =
                                    Validation.succeed Tuple.pair
                                        |> Validation.andMap (uuid |> Validation.map Uuid)
                                        |> Validation.andMap quantity
                                , view =
                                    \_ -> Div
                                }
                            )
                            |> Form.hiddenField "kind" (Field.exactValue "setQuantity" "Expected setQuantity")
                            |> Form.hiddenField "uuid" (Field.text |> Field.required "Required")
                            |> Form.field "quantity" (Field.int { invalid = \_ -> "Expected int" } |> Field.required "Required")
                        )
                        |> Form.Handler.with (\() -> Signout)
                            (Form.form
                                (\_ ->
                                    { combine = Validation.succeed ()
                                    , view = \_ -> Div
                                    }
                                )
                                |> Form.hiddenField "kind" (Field.exactValue "signout" "Expected signout")
                            )
            in
            [ test "first branch" <|
                \() ->
                    Form.Handler.run
                        (fields
                            [ ( "kind", "signout" )
                            ]
                        )
                        oneOfParsers
                        |> Expect.equal
                            (Valid Signout)
            , test "second branch" <|
                \() ->
                    Form.Handler.run
                        (fields
                            [ ( "kind", "setQuantity" )
                            , ( "uuid", "123" )
                            , ( "quantity", "1" )
                            ]
                        )
                        oneOfParsers
                        |> Expect.equal
                            (Valid (SetQuantity ( Uuid "123", 1 )))
            , test "3rd" <|
                \() ->
                    Form.Handler.run
                        (fields
                            [ ( "kind", "toggle-all" )
                            , ( "toggleTo", "" )
                            ]
                        )
                        todoForm
                        |> Expect.equal
                            (Valid (CheckAll False))

            --, test "no match" <|
            --    \() ->
            --        Form.runOneOfServerSide
            --            (fields [])
            --            oneOfParsers
            --            |> Expect.equal
            --                ( Nothing
            --                , Dict.fromList []
            --                )
            , describe "select" <|
                let
                    selectParser =
                        Form.form
                            (\media ->
                                { combine = media
                                , view =
                                    \_ -> Div
                                }
                            )
                            |> Form.field "media"
                                (Field.select
                                    [ ( "book", Book )
                                    , ( "article", Article )
                                    , ( "video", Video )
                                    ]
                                    (\_ -> "Invalid")
                                )
                in
                [ test "example" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "media", "book" )
                                ]
                            )
                            (selectParser |> Form.Handler.init identity)
                            |> Expect.equal
                                (Valid (Just Book))
                ]
            , describe "dependent validations" <|
                let
                    checkinFormParser : DoneForm String ( Date, Date ) input MyView
                    checkinFormParser =
                        Form.form
                            (\checkin checkout ->
                                { combine =
                                    Validation.succeed
                                        (\checkinValue checkoutValue ->
                                            Validation.succeed ( checkinValue, checkoutValue )
                                                |> (if Date.toRataDie checkinValue >= Date.toRataDie checkoutValue then
                                                        Validation.withError checkin "Must be before checkout"

                                                    else
                                                        identity
                                                   )
                                        )
                                        |> Validation.andMap checkin
                                        |> Validation.andMap checkout
                                        |> Validation.andThen identity
                                , view =
                                    \_ -> Div
                                }
                            )
                            |> Form.field "checkin"
                                (Field.date { invalid = \_ -> "Invalid" } |> Field.required "Required")
                            |> Form.field "checkout"
                                (Field.date { invalid = \_ -> "Invalid" } |> Field.required "Required")
                in
                [ test "checkin must be before checkout" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "checkin", "2022-01-01" )
                                , ( "checkout", "2022-01-03" )
                                ]
                            )
                            (checkinFormParser |> Form.Handler.init identity)
                            |> Expect.equal
                                (Valid ( Date.fromRataDie 738156, Date.fromRataDie 738158 ))
                , test "checkout is invalid because before checkin" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "checkin", "2022-01-03" )
                                , ( "checkout", "2022-01-01" )
                                ]
                            )
                            (checkinFormParser |> Form.Handler.init identity)
                            |> Expect.equal
                                (Invalid (Just ( Date.fromRataDie 738158, Date.fromRataDie 738156 ))
                                    (Dict.fromList
                                        [ ( "checkin", [ "Must be before checkout" ] )
                                        ]
                                    )
                                )
                , test "sub-form" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "password", "mypassword" )
                                , ( "password-confirmation", "doesnt-match" )
                                ]
                            )
                            (Form.form
                                (\postForm_ ->
                                    { combine =
                                        postForm_.combine ()
                                    , view =
                                        \_ -> ( [], [ Div ] )
                                    }
                                )
                                |> Form.dynamic
                                    (\() ->
                                        Form.form
                                            (\password passwordConfirmation ->
                                                { combine =
                                                    Validation.succeed
                                                        (\passwordValue passwordConfirmationValue ->
                                                            if passwordValue == passwordConfirmationValue then
                                                                Validation.succeed { password = passwordValue }

                                                            else
                                                                passwordConfirmation
                                                                    |> Validation.fail "Must match password"
                                                        )
                                                        |> Validation.andMap password
                                                        |> Validation.andMap passwordConfirmation
                                                        |> Validation.andThen identity
                                                , view = [ Div ]
                                                }
                                            )
                                            |> Form.field "password" (Field.text |> Field.password |> Field.required "Required")
                                            |> Form.field "password-confirmation" (Field.text |> Field.password |> Field.required "Required")
                                    )
                                |> Form.Handler.init identity
                            )
                            |> Expect.equal
                                (Invalid Nothing
                                    (Dict.fromList
                                        [ ( "password-confirmation", [ "Must match password" ] )
                                        ]
                                    )
                                )
                ]
            ]
        , describe "dependent parsing" <|
            let
                linkForm : DoneForm String PostAction input MyView
                linkForm =
                    Form.form
                        (\url ->
                            { combine =
                                Validation.succeed ParsedLink
                                    |> Validation.andMap url
                            , view =
                                \_ -> Div
                            }
                        )
                        |> Form.field "url"
                            (Field.text
                                |> Field.required "Required"
                                |> Field.url
                            )

                postForm : DoneForm String PostAction input MyView
                postForm =
                    Form.form
                        (\title body ->
                            { combine =
                                Validation.succeed
                                    (\titleValue bodyValue ->
                                        { title = titleValue
                                        , body = bodyValue
                                        }
                                    )
                                    |> Validation.andMap title
                                    |> Validation.andMap body
                                    |> Validation.map ParsedPost
                            , view = \_ -> Div
                            }
                        )
                        |> Form.field "title" (Field.text |> Field.required "Required")
                        |> Form.field "body" Field.text

                dependentParser : DoneForm String PostAction input MyView
                dependentParser =
                    Form.form
                        (\kind postForm_ ->
                            { combine =
                                kind
                                    |> Validation.andThen postForm_.combine
                            , view = \_ -> Div
                            }
                        )
                        |> Form.field "kind"
                            (Field.select
                                [ ( "link", Link )
                                , ( "post", Post )
                                ]
                                (\_ -> "Invalid")
                                |> Field.required "Required"
                            )
                        |> Form.dynamic
                            (\parsedKind ->
                                case parsedKind of
                                    Link ->
                                        linkForm

                                    Post ->
                                        postForm
                            )
            in
            [ test "parses link" <|
                \() ->
                    Form.Handler.run
                        (fields
                            [ ( "kind", "link" )
                            , ( "url", "https://elm-radio.com/episode/wrap-early-unwrap-late" )
                            ]
                        )
                        (dependentParser |> Form.Handler.init identity)
                        |> Expect.equal
                            (Valid (ParsedLink "https://elm-radio.com/episode/wrap-early-unwrap-late"))
            ]
        , describe "min/max validations" <|
            let
                minMaxForm : DoneForm String () input ()
                minMaxForm =
                    Form.form
                        (\date time int float range text ->
                            { combine = Validation.succeed ()
                            , view = \_ -> ()
                            }
                        )
                        |> Form.field "date"
                            (Field.date { invalid = \_ -> "Invalid" }
                                |> Field.required "Required"
                                |> Field.withMin (Date.fromRataDie 738157) ("Must be on or after " ++ (Date.fromRataDie 738157 |> Date.toIsoString))
                                |> Field.withMax (Date.fromRataDie 738167) ("Must be on or before " ++ (Date.fromRataDie 738167 |> Date.toIsoString))
                            )
                        |> Form.field "time"
                            (Field.time { invalid = \_ -> "Invalid" }
                                |> Field.withMin { hours = 10, minutes = 0, seconds = Nothing } "Must be at 10:00am or later"
                                |> Field.withMax { hours = 12, minutes = 0, seconds = Nothing } "Must be at 12:00pm or earlier"
                                |> Field.required "Required"
                            )
                        |> Form.field "int"
                            (Field.int { invalid = \_ -> "Invalid" }
                                |> Field.withMin 10 "Must be 10 or more"
                                |> Field.withMax 100 "Must be 100 or less"
                                |> Field.required "Required"
                            )
                        |> Form.field "float"
                            (Field.int { invalid = \_ -> "Invalid" }
                                |> Field.withMin 10 "Must be 10 or more"
                                |> Field.withMax 100 "Must be 100 or less"
                                |> Field.required "Required"
                            )
                        |> Form.field "range"
                            (Field.range
                                { min = 10
                                , max = 100
                                , missing = "Required"
                                , invalid = Debug.toString
                                }
                                (Field.int { invalid = \_ -> "Invalid" })
                            )
                        |> Form.field "text"
                            (Field.text
                                |> Field.required "Required"
                                |> Field.withMinLength 5 "Must be 5 characters or more"
                                |> Field.withMaxLength 10 "Must be 10 characters or less"
                            )
            in
            [ test "below min" <|
                \() ->
                    Form.Handler.run
                        (fields
                            [ ( "date", "2022-01-01" )
                            , ( "time", "9:00" )
                            , ( "int", "9" )
                            , ( "float", "9" )
                            , ( "range", "9" )
                            , ( "text", "1234" )
                            ]
                        )
                        (minMaxForm
                            |> Form.Handler.init identity
                        )
                        |> Expect.equal
                            (Invalid (Just ())
                                (Dict.fromList
                                    [ ( "date", [ "Must be on or after 2022-01-02" ] )
                                    , ( "time", [ "Must be at 10:00am or later" ] )
                                    , ( "int", [ "Must be 10 or more" ] )
                                    , ( "float", [ "Must be 10 or more" ] )
                                    , ( "range", [ "BelowRange" ] )
                                    , ( "text", [ "Must be 5 characters or more" ] )
                                    ]
                                )
                            )
            , test "above max" <|
                \() ->
                    Form.Handler.run
                        (fields
                            [ ( "date", "2022-01-13" )
                            , ( "time", "12:01" )
                            , ( "int", "101" )
                            , ( "float", "101" )
                            , ( "range", "101" )
                            , ( "text", "12345678901" )
                            ]
                        )
                        (minMaxForm
                            |> Form.Handler.init identity
                        )
                        |> Expect.equal
                            (Invalid (Just ())
                                (Dict.fromList
                                    [ ( "date", [ "Must be on or before 2022-01-12" ] )
                                    , ( "time", [ "Must be at 12:00pm or earlier" ] )
                                    , ( "int", [ "Must be 100 or less" ] )
                                    , ( "float", [ "Must be 100 or less" ] )
                                    , ( "range", [ "AboveRange" ] )
                                    , ( "text", [ "Must be 10 characters or less" ] )
                                    ]
                                )
                            )
            , test "min valid" <|
                \() ->
                    Form.Handler.run
                        (fields
                            [ ( "date", "2022-01-02" )
                            , ( "time", "10:00" )
                            , ( "int", "10" )
                            , ( "float", "10" )
                            , ( "range", "10" )
                            , ( "text", "12345" )
                            ]
                        )
                        (minMaxForm
                            |> Form.Handler.init identity
                        )
                        |> Expect.equal (Valid ())
            , test "max valid" <|
                \() ->
                    Form.Handler.run
                        (fields
                            [ ( "date", "2022-01-12" )
                            , ( "time", "12:00" )
                            , ( "int", "100" )
                            , ( "float", "100" )
                            , ( "range", "100" )
                            , ( "text", "1234567890" )
                            ]
                        )
                        (minMaxForm
                            |> Form.Handler.init identity
                        )
                        |> Expect.equal (Valid ())
            ]
        ]


type PostAction
    = ParsedLink String
    | ParsedPost { title : String, body : Maybe String }


type PostKind
    = Link
    | Post


type Media
    = Book
    | Article
    | Video


type MyView
    = Div


fields : List ( String, String ) -> List ( String, String )
fields list =
    list


todoForm : Form.Handler.Handler String TodoAction
todoForm =
    Form.Handler.init UpdateEntry editItemForm
        |> Form.Handler.with Add newItemForm
        |> Form.Handler.with Check completeItemForm
        |> Form.Handler.with Delete deleteItemForm
        |> Form.Handler.with (\_ -> DeleteComplete) clearCompletedForm
        |> Form.Handler.with CheckAll toggleAllForm


type TodoAction
    = UpdateEntry ( String, String )
    | Add String
    | Delete String
    | DeleteComplete
    | Check ( Bool, String )
    | CheckAll Bool


editItemForm : Form.HtmlForm String ( String, String ) input msg
editItemForm =
    Form.form
        (\itemId description ->
            { combine =
                Validation.succeed Tuple.pair
                    |> Validation.andMap itemId
                    |> Validation.andMap description
            , view = \_ -> []
            }
        )
        |> Form.hiddenField "itemId"
            (Field.text
                |> Field.required "Must be present"
            )
        |> Form.field "description"
            (Field.text
                |> Field.required "Must be present"
            )
        |> Form.hiddenKind ( "kind", "edit-item" ) "Expected kind"


newItemForm : Form.HtmlForm String String input msg
newItemForm =
    Form.form
        (\description ->
            { combine =
                Validation.succeed identity
                    |> Validation.andMap description
            , view = \_ -> []
            }
        )
        |> Form.field "description" (Field.text |> Field.required "Must be present")
        |> Form.hiddenKind ( "kind", "new-item" ) "Expected kind"


completeItemForm : Form.HtmlForm String ( Bool, String ) input msg
completeItemForm =
    Form.form
        (\todoId complete ->
            { combine =
                Validation.succeed Tuple.pair
                    |> Validation.andMap complete
                    |> Validation.andMap todoId
            , view = \_ -> []
            }
        )
        |> Form.hiddenField "todoId"
            (Field.text
                |> Field.required "Must be present"
            )
        |> Form.hiddenField "complete" Field.checkbox
        |> Form.hiddenKind ( "kind", "complete" ) "Expected kind"


deleteItemForm : Form.HtmlForm String String input msg
deleteItemForm =
    Form.form
        (\todoId ->
            { combine =
                Validation.succeed identity
                    |> Validation.andMap todoId
            , view = \_ -> []
            }
        )
        |> Form.hiddenField "todoId"
            (Field.text
                |> Field.required "Must be present"
             --|> Field.withInitialValue (.id >> Form.Value.string)
            )
        |> Form.hiddenKind ( "kind", "delete" ) "Expected kind"


clearCompletedForm : Form.HtmlForm String () { entriesCompleted : Int } msg
clearCompletedForm =
    Form.form
        { combine = Validation.succeed ()
        , view = \_ -> []
        }
        |> Form.hiddenKind ( "kind", "clear-completed" ) "Expected kind"


toggleAllForm : Form.HtmlForm String Bool input msg
toggleAllForm =
    Form.form
        (\toggleTo ->
            { combine =
                Validation.succeed identity
                    |> Validation.andMap toggleTo
            , view = \_ -> []
            }
        )
        |> Form.hiddenField "toggleTo" Field.checkbox
        |> Form.hiddenKind ( "kind", "toggle-all" ) "Expected kind"

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


type UserAdminFormType
    = UserForm
    | AdminForm


type EmployeeContractorFormType
    = EmployeeForm
    | ContractorForm


type EmployeeContractorFormType2
    = EmployeeForm2
    | ContractorForm2


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
            , test "includes dynamic form errors" <|
                \() ->
                    Form.Handler.run
                        (fields
                            [ ( "kind", "link" )
                            ]
                        )
                        (dependentParser |> Form.Handler.init identity)
                        |> Expect.equal
                            (Invalid Nothing
                                (Dict.fromList
                                    [ ( "url", [ "Required" ] )
                                    ]
                                )
                            )
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
        , describe "Dynamic form error scenarios" <|
            let
                -- Test form with dynamic sub-forms that can have both field errors and validation errors
                dynamicFormWithErrors : DoneForm String String input MyView
                dynamicFormWithErrors =
                    Form.form
                        (\formType subForm ->
                            { combine =
                                formType
                                    |> Validation.andThen subForm.combine
                            , view = \_ -> Div
                            }
                        )
                        |> Form.field "formType"
                            (Field.select
                                [ ( "user", UserForm )
                                , ( "admin", AdminForm )
                                ]
                                (\_ -> "Invalid form type")
                                |> Field.required "Form type is required"
                            )
                        |> Form.dynamic
                            (\parsedType ->
                                case parsedType of
                                    UserForm ->
                                        userForm

                                    AdminForm ->
                                        adminForm
                            )
            in
            [ describe "Initial form errors AND follow-up form errors in final result" <|
                [ test "User form with errors in both initial and follow-up forms" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "formType", "user" )
                                , ( "username", "ab" ) -- Too short (follow-up form validation error)
                                , ( "email", "invalid-email" ) -- Invalid email (follow-up form validation error)
                                , ( "age", "16" ) -- Too young (follow-up form validation error)
                                ]
                            )
                            (dynamicFormWithErrors |> Form.Handler.init identity)
                            |> Expect.equal
                                (Invalid Nothing
                                    (Dict.fromList
                                        [ ( "username", [ "Username too short" ] ) -- Follow-up form validation error
                                        , ( "email", [ "Invalid email format" ] ) -- Follow-up form validation error
                                        , ( "age", [ "Must be 18 or older" ] ) -- Follow-up form validation error
                                        ]
                                    )
                                )
                , test "Admin form with errors in both initial and follow-up forms" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "formType", "admin" )
                                , ( "adminCode", "WRONG123" ) -- Invalid code (follow-up form validation error)
                                , ( "permissions", "ab" ) -- Too short permissions (follow-up form validation error)
                                , ( "level", "3" ) -- Too low level (follow-up form validation error)
                                ]
                            )
                            (dynamicFormWithErrors |> Form.Handler.init identity)
                            |> Expect.equal
                                (Invalid Nothing
                                    (Dict.fromList
                                        [ ( "adminCode", [ "Invalid admin code" ] ) -- Follow-up form validation error
                                        , ( "permissions", [ "Permissions must be at least 3 characters" ] ) -- Follow-up form validation error
                                        , ( "level", [ "Admin level must be 5 or higher" ] ) -- Follow-up form validation error
                                        ]
                                    )
                                )
                , test "Multiple field errors in initial form, multiple validation errors in follow-up" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "formType", "user" )

                                -- Missing username and email (multiple initial form errors)
                                , ( "age", "15" ) -- Too young (follow-up form validation error)
                                ]
                            )
                            (dynamicFormWithErrors |> Form.Handler.init identity)
                            |> Expect.equal
                                (Invalid Nothing
                                    (Dict.fromList
                                        [ ( "email", [ "Email required" ] ) -- Initial form error
                                        , ( "username", [ "Username required" ] ) -- Initial form error
                                        ]
                                    )
                                )
                ]
            , describe "Validation errors in combine with permutations of initial and follow-up forms" <|
                let
                    -- Form with complex validation that combines multiple field and validation errors
                    complexValidationForm : DoneForm String String input MyView
                    complexValidationForm =
                        Form.form
                            (\formType subForm ->
                                { combine =
                                    formType
                                        |> Validation.andThen subForm.combine
                                , view = \_ -> Div
                                }
                            )
                            |> Form.field "formType"
                                (Field.select
                                    [ ( "employee", EmployeeForm )
                                    , ( "contractor", ContractorForm )
                                    ]
                                    (\_ -> "Invalid form type")
                                    |> Field.required "Form type is required"
                                )
                            |> Form.dynamic
                                (\parsedType ->
                                    case parsedType of
                                        EmployeeForm ->
                                            employeeForm

                                        ContractorForm ->
                                            contractorForm
                                )
                in
                [ test "Employee form with validation errors only in follow-up form" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "formType", "employee" )
                                , ( "name", "John" )
                                , ( "department", "Intern" )
                                , ( "salary", "150000" ) -- High salary for intern (validation error)
                                , ( "startDate", "2024-01-01" )
                                ]
                            )
                            (complexValidationForm |> Form.Handler.init identity)
                            |> Expect.equal
                                (Invalid Nothing
                                    (Dict.fromList
                                        [ ( "salary", [ "Interns cannot earn more than $100,000" ] )
                                        ]
                                    )
                                )
                , test "Contractor form with validation errors only in follow-up form" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "formType", "contractor" )
                                , ( "company", "Acme Corp" )
                                , ( "rate", "30" ) -- Low rate
                                , ( "hours", "35" ) -- High hours with low rate (validation error)
                                , ( "contractEnd", "2024-12-31" )
                                ]
                            )
                            (complexValidationForm |> Form.Handler.init identity)
                            |> Expect.equal
                                (Invalid Nothing
                                    (Dict.fromList
                                        [ ( "rate", [ "High hours require premium rate" ] )
                                        ]
                                    )
                                )
                , test "Employee form with errors in initial form and validation errors in follow-up" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "formType", "employee" )
                                , ( "name", "A" ) -- Too short (field validation error)
                                , ( "department", "Intern" )
                                , ( "salary", "150000" ) -- High salary for intern (combine validation error)
                                , ( "startDate", "2024-01-01" )
                                ]
                            )
                            (complexValidationForm |> Form.Handler.init identity)
                            |> Expect.equal
                                (Invalid Nothing
                                    (Dict.fromList
                                        [ ( "name", [ "Name must be at least 2 characters" ] )
                                        ]
                                    )
                                )
                , test "Contractor form with errors in initial form and validation errors in follow-up" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "formType", "contractor" )
                                , ( "company", "ab" ) -- Too short company name (field validation error)
                                , ( "rate", "30" )
                                , ( "hours", "35" ) -- High hours with low rate (combine validation error)
                                , ( "contractEnd", "2024-12-31" )
                                ]
                            )
                            (complexValidationForm |> Form.Handler.init identity)
                            |> Expect.equal
                                (Invalid Nothing
                                    (Dict.fromList
                                        [ ( "company", [ "Company name must be at least 3 characters" ] )
                                        ]
                                    )
                                )
                ]
            , describe "Combination of field errors and Validation errors in permutations" <|
                let
                    complexValidationForm : DoneForm String String input MyView
                    complexValidationForm =
                        Form.form
                            (\formType subForm ->
                                { combine =
                                    formType
                                        |> Validation.andThen subForm.combine
                                , view = \_ -> Div
                                }
                            )
                            |> Form.field "formType"
                                (Field.select
                                    [ ( "employee", EmployeeForm2 )
                                    , ( "contractor", ContractorForm2 )
                                    ]
                                    (\_ -> "Invalid form type")
                                    |> Field.required "Form type is required"
                                )
                            |> Form.dynamic
                                (\parsedType ->
                                    case parsedType of
                                        EmployeeForm2 ->
                                            employeeForm

                                        ContractorForm2 ->
                                            contractorForm
                                )
                in
                [ test "Employee form with all error types" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "formType", "employee" )
                                , ( "name", "A" ) -- Field validation error (too short)
                                , ( "department", "a" ) -- Field validation error (too short)
                                , ( "salary", "150000" ) -- Combine validation error (high salary for intern)
                                , ( "startDate", "2024-01-01" )
                                ]
                            )
                            (complexValidationForm |> Form.Handler.init identity)
                            |> Expect.equal
                                (Invalid Nothing
                                    (Dict.fromList
                                        [ ( "name", [ "Name must be at least 2 characters" ] ) -- Field validation error
                                        , ( "department", [ "Department must be at least 2 characters" ] ) -- Field validation error
                                        ]
                                    )
                                )
                , test "Contractor form with all error types" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "formType", "contractor" )
                                , ( "company", "Acme Corp" )
                                , ( "rate", "20" ) -- Field validation error (too low)
                                , ( "hours", "45" ) -- Field validation error (too high)
                                , ( "contractEnd", "2024-12-31" )
                                ]
                            )
                            (complexValidationForm |> Form.Handler.init identity)
                            |> Expect.equal
                                (Invalid Nothing
                                    (Dict.fromList
                                        [ ( "rate", [ "Hourly rate must be at least $25" ] )
                                        , ( "hours", [ "Hours must be between 1 and 40" ] )
                                        ]
                                    )
                                )
                , test "Complex scenario with multiple error types across both forms" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "formType", "contractor" )
                                , ( "company", "ab" ) -- Field validation error (too short)
                                , ( "rate", "20" ) -- Field validation error (too low)
                                , ( "hours", "35" ) -- Valid hours
                                , ( "contractEnd", "2024-12-31" )
                                ]
                            )
                            (complexValidationForm |> Form.Handler.init identity)
                            |> Expect.equal
                                (Invalid Nothing
                                    (Dict.fromList
                                        [ ( "company", [ "Company name must be at least 3 characters" ] ) -- Field validation error
                                        , ( "rate", [ "Hourly rate must be at least $25" ] ) -- Field validation error
                                        ]
                                    )
                                )
                , test "Employee form with overlapping field and combine validation errors" <|
                    \() ->
                        Form.Handler.run
                            (fields
                                [ ( "formType", "employee" )
                                , ( "name", "John" )
                                , ( "department", "Intern" )
                                , ( "salary", "5000" ) -- Field validation error (too low) AND combine validation error (intern salary limit)
                                , ( "startDate", "2024-01-01" )
                                ]
                            )
                            (complexValidationForm |> Form.Handler.init identity)
                            |> Expect.equal
                                (Invalid Nothing
                                    (Dict.fromList
                                        [ ( "salary", [ "Salary must be at least $30,000" ] ) -- Field validation error
                                        ]
                                    )
                                )
                ]
            ]
        ]


userForm : DoneForm String String input MyView
userForm =
    Form.form
        (\username email age ->
            { combine =
                Validation.map3
                    (\user emailValue ageValue ->
                        let
                            usernameErrors =
                                if String.length user < 3 then
                                    Validation.fail "Username too short" username

                                else
                                    Validation.succeed user

                            emailErrors =
                                if not (String.contains "@" emailValue) then
                                    Validation.fail "Invalid email format" email

                                else
                                    Validation.succeed emailValue

                            ageErrors =
                                if ageValue < 18 then
                                    Validation.fail "Must be 18 or older" age

                                else
                                    Validation.succeed ageValue
                        in
                        Validation.map3
                            (\u e a ->
                                Validation.succeed ("User: " ++ u ++ " (" ++ e ++ ", age " ++ String.fromInt a ++ ")")
                            )
                            usernameErrors
                            emailErrors
                            ageErrors
                            |> Validation.andThen identity
                    )
                    username
                    email
                    age
                    |> Validation.andThen identity
            , view = \_ -> Div
            }
        )
        |> Form.field "username" (Field.text |> Field.required "Username required")
        |> Form.field "email" (Field.text |> Field.required "Email required")
        |> Form.field "age" (Field.int { invalid = \_ -> "Invalid age" } |> Field.required "Age required")


adminForm : DoneForm String String input MyView
adminForm =
    Form.form
        (\adminCode permissions level ->
            { combine =
                Validation.map3
                    (\code perms levelValue ->
                        let
                            codeErrors =
                                if code /= "ADMIN123" then
                                    Validation.fail "Invalid admin code" adminCode

                                else
                                    Validation.succeed code

                            permsErrors =
                                if String.length perms < 3 then
                                    Validation.fail "Permissions must be at least 3 characters" permissions

                                else
                                    Validation.succeed perms

                            levelErrors =
                                if levelValue < 5 then
                                    Validation.fail "Admin level must be 5 or higher" level

                                else
                                    Validation.succeed levelValue
                        in
                        Validation.map3
                            (\c p l ->
                                Validation.succeed ("Admin: " ++ c ++ " (" ++ p ++ ", level " ++ String.fromInt l ++ ")")
                            )
                            codeErrors
                            permsErrors
                            levelErrors
                            |> Validation.andThen identity
                    )
                    adminCode
                    permissions
                    level
                    |> Validation.andThen identity
            , view = \_ -> Div
            }
        )
        |> Form.field "adminCode" (Field.text |> Field.required "Admin code required")
        |> Form.field "permissions" (Field.text |> Field.required "Permissions required")
        |> Form.field "level" (Field.int { invalid = \_ -> "Invalid level" } |> Field.required "Level required")


employeeForm : DoneForm String String input MyView
employeeForm =
    Form.form
        (\name department salary startDate ->
            { combine =
                Validation.map4
                    (\nameValue dept salaryValue date ->
                        let
                            nameErrors =
                                if String.length nameValue < 2 then
                                    Validation.fail "Name must be at least 2 characters" name

                                else
                                    Validation.succeed nameValue

                            deptErrors =
                                if String.length dept < 2 then
                                    Validation.fail "Department must be at least 2 characters" department

                                else
                                    Validation.succeed dept

                            salaryErrors =
                                if salaryValue < 30000 then
                                    Validation.fail "Salary must be at least $30,000" salary

                                else
                                    Validation.succeed salaryValue

                            dateErrors =
                                if String.isEmpty date then
                                    Validation.fail "Start date is required" startDate

                                else
                                    Validation.succeed date
                        in
                        Validation.map4
                            (\n d s dt ->
                                let
                                    combineErrors =
                                        if s > 100000 && d == "Intern" then
                                            Validation.fail "Interns cannot earn more than $100,000" salary

                                        else
                                            Validation.succeed s
                                in
                                Validation.map2
                                    (\salVal _ ->
                                        Validation.succeed ("Employee: " ++ n ++ " in " ++ d ++ " earning $" ++ String.fromInt salVal ++ " starting " ++ dt)
                                    )
                                    combineErrors
                                    (Validation.succeed ())
                                    |> Validation.andThen identity
                            )
                            nameErrors
                            deptErrors
                            salaryErrors
                            dateErrors
                            |> Validation.andThen identity
                    )
                    name
                    department
                    salary
                    startDate
                    |> Validation.andThen identity
            , view = \_ -> Div
            }
        )
        |> Form.field "name" (Field.text |> Field.required "Name required")
        |> Form.field "department" (Field.text |> Field.required "Department required")
        |> Form.field "salary" (Field.int { invalid = \_ -> "Invalid salary" } |> Field.required "Salary required")
        |> Form.field "startDate" (Field.text |> Field.required "Start date required")


contractorForm : DoneForm String String input MyView
contractorForm =
    Form.form
        (\company rate hours contractEnd ->
            { combine =
                Validation.map4
                    (\comp rateValue hoursValue endDate ->
                        let
                            companyErrors =
                                if String.length comp < 3 then
                                    Validation.fail "Company name must be at least 3 characters" company

                                else
                                    Validation.succeed comp

                            rateErrors =
                                if rateValue < 25 then
                                    Validation.fail "Hourly rate must be at least $25" rate

                                else
                                    Validation.succeed rateValue

                            hoursErrors =
                                if hoursValue < 1 || hoursValue > 40 then
                                    Validation.fail "Hours must be between 1 and 40" hours

                                else
                                    Validation.succeed hoursValue

                            endDateErrors =
                                if String.isEmpty endDate then
                                    Validation.fail "Contract end date is required" contractEnd

                                else
                                    Validation.succeed endDate
                        in
                        Validation.map4
                            (\c r h ed ->
                                let
                                    combineErrors =
                                        if h > 30 && r < 50 then
                                            Validation.fail "High hours require premium rate" rate

                                        else
                                            Validation.succeed r
                                in
                                Validation.map2
                                    (\rateVal _ ->
                                        Validation.succeed ("Contractor: " ++ c ++ " at $" ++ String.fromInt rateVal ++ "/hr for " ++ String.fromInt h ++ " hours ending " ++ ed)
                                    )
                                    combineErrors
                                    (Validation.succeed ())
                                    |> Validation.andThen identity
                            )
                            companyErrors
                            rateErrors
                            hoursErrors
                            endDateErrors
                            |> Validation.andThen identity
                    )
                    company
                    rate
                    hours
                    contractEnd
                    |> Validation.andThen identity
            , view = \_ -> Div
            }
        )
        |> Form.field "company" (Field.text |> Field.required "Company required")
        |> Form.field "rate" (Field.int { invalid = \_ -> "Invalid rate" } |> Field.required "Rate required")
        |> Form.field "hours" (Field.int { invalid = \_ -> "Invalid hours" } |> Field.required "Hours required")
        |> Form.field "contractEnd" (Field.text |> Field.required "Contract end date required")


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

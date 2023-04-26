module Example exposing (all)

--import Http
--import Random

import Browser
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html
import Html.Attributes exposing (for, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode
import ProgramTest exposing (ProgramTest, clickButton, expectViewHas, fillIn, update)
import SimulatedEffect.Cmd
import SimulatedEffect.Http as Http
import SimulatedEffect.Sub
import Test exposing (..)
import Test.Html.Selector exposing (text)



--import Test.Runner.Html


type Model
    = Form
        { name : String
        , streetAddress : String
        , postalCode : String
        , error : Maybe String
        }
    | Loading
    | Success
        { nextElection : String
        }


type Msg
    = ChangeName String
    | ChangeStreetAddress String
    | ChangePostalCode String
    | SubmitRegistration
    | RegistrationResponse (Result Http.Error String)


type alias Flags =
    ()



--init : Flags -> ( Model, Cmd Msg )


init () =
    ( Form
        { name = ""
        , streetAddress = ""
        , postalCode = ""
        , error = Nothing
        }
    , Cmd.none
    )



--update : Msg -> Model -> ( Model, Cmd Msg )


none =
    Cmd.none


update msg model =
    case ( model, msg ) of
        ( Form info, ChangeName newName ) ->
            ( Form { info | name = newName }
            , none
            )

        ( Form info, ChangeStreetAddress newStreetAddress ) ->
            ( Form { info | streetAddress = newStreetAddress }
            , none
            )

        ( Form info, ChangePostalCode newPostalCode ) ->
            ( Form { info | postalCode = newPostalCode }
            , none
            )

        ( Form info, SubmitRegistration ) ->
            let
                isValid =
                    String.length info.postalCode == 5
            in
            if isValid then
                ( Loading
                , --Http.post
                  --    { url = "/register"
                  --    , body =
                  --        Http.jsonBody <|
                  --            Json.Encode.object
                  --                [ ( "name", Json.Encode.string info.name )
                  --                , ( "streetAddress", Json.Encode.string info.streetAddress )
                  --                , ( "postalCode", Json.Encode.string info.postalCode )
                  --                ]
                  --    , expect =
                  --        Http.expectJson
                  --            RegistrationResponse
                  --            (Json.Decode.field "nextElection" Json.Decode.string)
                  --    }
                  none
                )

            else
                ( Form { info | error = Just "You must enter a valid postal code" }
                , none
                )

        ( Form _, _ ) ->
            ( model, none )

        ( Loading, RegistrationResponse (Ok nextElection) ) ->
            ( Success
                { nextElection = nextElection
                }
            , none
            )

        ( Loading, _ ) ->
            ( model, none )

        ( Success _, _ ) ->
            ( model, none )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ case model of
            Form info ->
                -- TODO: should use onSubmit instead of an onClick button
                Html.form []
                    [ Html.label
                        [ for "name" ]
                        [ Html.text "Name" ]
                    , Html.input
                        [ id "name"
                        , onInput ChangeName
                        , value info.name
                        ]
                        []
                    , Html.label
                        [ for "street-address" ]
                        [ Html.text "Street Address" ]
                    , Html.input
                        [ id "street-address"
                        , onInput ChangeStreetAddress
                        , value info.streetAddress
                        ]
                        []
                    , Html.label
                        [ for "postcode" ]
                        [ Html.text "Postal Code" ]
                    , Html.input
                        [ id "postcode"
                        , onInput ChangePostalCode
                        , value info.postalCode
                        ]
                        []
                    , case info.error of
                        Nothing ->
                            Html.text ""

                        Just error ->
                            Html.text error
                    , Html.button
                        [ onClick SubmitRegistration
                        , type_ "button"
                        ]
                        [ Html.text "Register" ]
                    ]

            Loading ->
                Html.text "Loading..."

            Success info ->
                Html.div []
                    [ Html.text "Success!"
                    , Html.hr [] []
                    , Html.text ("Next election date is: " ++ info.nextElection)
                    ]
        ]
    }


start =
    ProgramTest.createDocument
        { init = init
        , update = update
        , view = view
        }
        |> ProgramTest.start ()


all : Test
all =
    describe "voter registration frontend"
        [ test "happy path: successful registration" <|
            \() ->
                start
                    |> fillIn "name" "Name" "Bailey Sheppard"
                    |> fillIn "street-address" "Street Address" "14 North Moore Street"
                    |> fillIn "postcode" "Postal Code" "60606"
                    |> clickButton "Register"
                    |> ProgramTest.update (RegistrationResponse (Ok "Aug 12"))
                    |> expectViewHas
                        [ text "Success!"
                        , text "Next election date is: Aug 12"
                        ]
        , test "invalid postal code shows a validation error" <|
            \() ->
                start
                    |> fillIn "name" "Name" "Bailey Sheppard"
                    |> fillIn "street-address" "Street Address" "14 North Moore Street"
                    |> fillIn "postcode" "Postal Code" "0000"
                    |> clickButton "Register"
                    |> expectViewHas
                        [ text "You must enter a valid postal code"
                        ]
        ]

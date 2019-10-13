module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, h3, li, text, ul)
import Html.Attributes as Attrs exposing (..)
import Http
import Json.Decode as JD exposing (Decoder, field, list, map2, string)
import List
import Task
import Time exposing (..)



---- MODEL ----


type RequestStatus
    = Loading
    | Failure
    | Success (List Menu)


type alias Model =
    { envs : Flags
    , date : Maybe Date
    , status : RequestStatus
    , menus : List Menu
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { envs = flags, date = Nothing, status = Loading, menus = [] }
    , currentDate
    )


type alias Flags =
    { apiUrl : String }


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


type alias Menu =
    { restaurant : String
    , foods : List MenuItem
    }


type alias MenuItem =
    { title : String }



---- UPDATE ----


type Msg
    = GetMenus
    | ResultReceived (Result Http.Error (List Menu))
    | SetTime Time.Posix
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetMenus ->
            case model.date of
                Just date ->
                    ( model, getMenus date model.envs.apiUrl )

                Nothing ->
                    ( model, Cmd.none )

        ResultReceived result ->
            case result of
                Ok menus ->
                    ( { model | status = Success menus, menus = menus }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        SetTime time ->
            let
                dateFromTime =
                    { year = Time.toYear Time.utc time, month = monthToNumber (Time.toMonth Time.utc time), day = Time.toDay Time.utc time }
            in
            ( { model | date = Just dateFromTime }, getMenus dateFromTime model.envs.apiUrl )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ id "root" ]
        [ div [ class "main-wrap" ]
            [ h1 [] [ text "Lounaspaikat Ruoholahdessa 🍔🍟🍕\u{1F32E}\u{1F959}" ]
            , h3 [] [ text (dateToString model.date) ]
            , viewMenus model
            ]
        ]


viewMenus : Model -> Html Msg
viewMenus model =
    case model.status of
        Loading ->
            div [] [ text "Loading ..." ]

        Failure ->
            div [] [ text "Failure" ]

        Success _ ->
            if List.isEmpty model.menus then
                text "Ei ruokalistoja tälle päivälle 😪"

            else
                div [] (List.map viewMenu model.menus)


viewMenu : Menu -> Html Msg
viewMenu menu =
    let
        tableRow menuItem =
            li [ class "menu-item" ] [ text menuItem.title ]
    in
    div [ class "restaurant" ]
        [ div [ class "titleBar" ]
            [ h2 [] [ text menu.restaurant ]
            ]
        , ul []
            (List.map
                tableRow
                menu.foods
            )
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



-- HTTP


getMenus : Date -> String -> Cmd Msg
getMenus date apiUrl =
    let
        dateString =
            String.fromInt date.year ++ "-" ++ String.fromInt date.month ++ "-" ++ String.fromInt date.day
    in
    Http.get
        { url = apiUrl ++ "api/" ++ dateString
        , expect = Http.expectJson ResultReceived menuDecoder
        }


menuDecoder : Decoder (List Menu)
menuDecoder =
    list (JD.map2 Menu (field "restaurant" string) (field "foods" (list (JD.map MenuItem (field "title" string)))))



-- UTILS


dateToString : Maybe Date -> String
dateToString date =
    case date of
        Just datee ->
            String.fromInt datee.day ++ "." ++ String.fromInt datee.month ++ "." ++ String.fromInt datee.year

        Nothing ->
            "No date"


currentDate : Cmd Msg
currentDate =
    Task.perform SetTime Time.now


monthToNumber : Time.Month -> Int
monthToNumber m =
    case m of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12

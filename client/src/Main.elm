module Main exposing (..)

import Browser
import Debug exposing (log)
import Html exposing (Html, div, h1, h2, img, li, p, table, td, text, tr, ul)
import Html.Attributes exposing (src)
import Http
import Json.Decode as JD exposing (Decoder, field, index, list, map2, string)
import List
import Maybe exposing (..)
import Task
import Time exposing (..)



---- MODEL ----


type RequestStatus
    = Loading
    | Failure
    | Success (List Menu)


type alias Model =
    { date : Maybe Date
    , status : RequestStatus
    , menus : List Menu
    }


init : ( Model, Cmd Msg )
init =
    let
        initialDate =
            "moro"
    in
    ( { date = Nothing, status = Loading, menus = [] }
    , currentDate
    )


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


type alias Menu =
    { restaurant : String, foods : List MenuItem }


type alias MenuItem =
    { title : String, itemPrice : String }



---- UPDATE ----


type Msg
    = GetMenus
    | ResultReceived (Result Http.Error (List Menu))
    | SetTime Time.Posix
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "moro" model
    in
    case msg of
        GetMenus ->
            case model.date of
                Just date ->
                    ( model, getMenus date )

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
            ( { model | date = Just dateFromTime }, getMenus dateFromTime )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Lounaspaikat Ruoholahdessa 🍔🍟🍕\u{1F32E}\u{1F959}" ]
        , h2 [] [ text (dateToString model.date) ]
        , viewMenus model
        ]


viewMenus : Model -> Html Msg
viewMenus model =
    case model.status of
        Loading ->
            div [] [ text "Loading ..." ]

        Failure ->
            div [] [ text "Failure" ]

        Success result ->
            if List.isEmpty model.menus then
                text "Ei ruokalistoja tälle päivälle 😪"

            else
                div [] (List.map viewMenu model.menus)


viewMenu : Menu -> Html Msg
viewMenu menu =
    let
        tableRow menuItem =
            tr [] [ td [] [ text menuItem.title ], td [] [ text menuItem.itemPrice ] ]
    in
    div []
        [ h1 []
            [ text menu.restaurant ]
        , table []
            (List.map
                tableRow
                menu.foods
            )
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



-- HTTP


backendUrl =
    "http://localhost:3001/api/"


getMenus : Date -> Cmd Msg
getMenus date =
    let
        dateString =
            String.fromInt date.year ++ "-" ++ String.fromInt date.month ++ "-" ++ String.fromInt date.day
    in
    Http.get
        { url = backendUrl ++ dateString
        , expect = Http.expectJson ResultReceived menuDecoder
        }


menuDecoder : Decoder (List Menu)
menuDecoder =
    list (JD.map2 Menu (field "restaurant" string) (field "foods" (list (JD.map2 MenuItem (field "title" string) (field "itemPrice" string)))))



-- list (field "restaurant" string)
-- UTILS


dateToString : Maybe Date -> String
dateToString date =
    case date of
        Just datee ->
            String.fromInt datee.day ++ "." ++ String.fromInt datee.month ++ "." ++ String.fromInt datee.year

        Nothing ->
            "No date"


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

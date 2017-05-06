module Sundial exposing (..)

import Sundial.Entry as Entry exposing (..)
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Task
import Time exposing (Time, second)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { description : String
    , duration : Time
    , startDate : Maybe Date
    , storedEntries : List Entry
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" 0 Nothing [], Cmd.none )



-- UPDATE


type Msg
    = ToggleTimer
    | SetDescription String
    | StartTimer Date
    | StopTimer Date Date
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleTimer ->
            case model.startDate of
                Just startDate ->
                    ( model, Task.perform (StopTimer startDate) Date.now )

                Nothing ->
                    ( model, Task.perform StartTimer Date.now )

        SetDescription newDescription ->
            ( { model | description = newDescription }, Cmd.none )

        StartTimer startDate_ ->
            ( { model | startDate = Just startDate_ }, Cmd.none )

        StopTimer startDate endDate ->
            ( storeEntry model startDate endDate, Cmd.none )

        Tick _ ->
            ( { model | duration = model.duration + second }, Cmd.none )


storeEntry : Model -> Date -> Date -> Model
storeEntry model startDate endDate =
    let
        newDateRangeEntry =
            DateRangeEntry
                (DateRangeEntryInfo
                    model.description
                    startDate
                    endDate
                )
    in
        { model
            | description = ""
            , duration = 0
            , startDate = Nothing
            , storedEntries = newDateRangeEntry :: model.storedEntries
        }



-- SUBSCRIPTONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.startDate of
        Just _ ->
            Time.every second Tick

        Nothing ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit ToggleTimer ]
            [ (input
                [ placeholder "Enter a description"
                , value model.description
                , onInput SetDescription
                ]
                []
              )
            , viewButton model.startDate
            ]
        , h4 []
            [ text
                ("Time Elapsed: " ++ viewTimeElapsed model.duration)
            ]
        , viewEntries model.storedEntries
        ]


type TimeUnit
    = Hour
    | Minute
    | Second


viewTimeElapsed : Time -> String
viewTimeElapsed time =
    viewTimeUnit time Hour
        ++ ":"
        ++ viewTimeUnit time Minute
        ++ ":"
        ++ viewTimeUnit time Second


viewTimeUnit : Time -> TimeUnit -> String
viewTimeUnit time timeUnit =
    case timeUnit of
        Hour ->
            toString (round time // round Time.hour)

        Minute ->
            maybePad ((round time // round Time.minute) % 60)

        Second ->
            maybePad ((round time // round Time.second) % 60)


maybePad : Int -> String
maybePad time =
    if time < 10 then
        "0" ++ toString time
    else
        toString time


viewButton : Maybe Date -> Html Msg
viewButton startDate =
    let
        buttonText =
            case startDate of
                Just _ ->
                    "Stop"

                Nothing ->
                    "Start"
    in
        input [ type_ "submit", value buttonText ] []


viewEntries : List Entry -> Html Msg
viewEntries entryList =
    div [] [ ol [] (List.map viewEntry entryList) ]


viewEntry : Entry -> Html Msg
viewEntry entry =
    li []
        [ text
            ((viewTimeElapsed (entryDuration entry))
                ++ " - "
                ++ entryDescription entry
            )
        ]

module Sundial exposing (..)

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
    { running : Bool
    , currentEntry : Entry
    , storedEntries : List Entry
    }


type alias Entry =
    { description : String
    , startDate : Maybe Date
    , endDate : Maybe Date
    }


init : ( Model, Cmd Msg )
init =
    let
        entry =
            Entry "" Nothing Nothing
    in
        ( Model False entry [], Cmd.none )



-- UPDATE


type Msg
    = ToggleTimer
    | SetDescription String
    | StartTimer Date
    | StopTimer Date
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleTimer ->
            if model.running then
                ( model, Task.perform StopTimer Date.now )
            else
                ( model, Task.perform StartTimer Date.now )

        SetDescription newDescription ->
            let
                newCurrentEntry =
                    Entry
                        newDescription
                        model.currentEntry.startDate
                        model.currentEntry.endDate
            in
                ( { model | currentEntry = newCurrentEntry }, Cmd.none )

        StartTimer startDate_ ->
            let
                newCurrentEntry =
                    Entry model.currentEntry.description (Just startDate_) Nothing
            in
                ( { model
                    | currentEntry = newCurrentEntry
                    , running = True
                  }
                , Cmd.none
                )

        StopTimer endDate ->
            let
                newStoredEntries =
                    Entry
                        model.currentEntry.description
                        model.currentEntry.startDate
                        (Just endDate)
                        :: model.storedEntries
            in
                ( Model
                    False
                    (Entry "" Nothing Nothing)
                    newStoredEntries
                , Cmd.none
                )

        Tick currentTime ->
            let
                newCurrentEntry =
                    Entry
                        model.currentEntry.description
                        model.currentEntry.startDate
                        (Just (Date.fromTime currentTime))
            in
                ( { model | currentEntry = newCurrentEntry }, Cmd.none )


dateDiff : Maybe Date -> Maybe Date -> Time
dateDiff startDate endDate =
    case ( startDate, endDate ) of
        ( Just startDate_, Just endDate_ ) ->
            Date.toTime endDate_ - Date.toTime startDate_

        ( _, _ ) ->
            0



-- SUBSCRIPTONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every second Tick
    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit ToggleTimer ]
            [ (input
                [ placeholder "Enter a description"
                , value model.currentEntry.description
                , onInput SetDescription
                ]
                []
              )
            , viewButton model
            ]
        , h4 []
            [ text
                ("Time Elapsed: "
                    ++ (viewTimeElapsed
                            model.currentEntry.startDate
                            model.currentEntry.endDate
                       )
                )
            ]
        , viewEntries model
        ]


type TimeUnit
    = Hour
    | Minute
    | Second


viewTimeElapsed : Maybe Date -> Maybe Date -> String
viewTimeElapsed startDate endDate =
    let
        time =
            dateDiff startDate endDate
    in
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


viewButton : Model -> Html Msg
viewButton model =
    let
        buttonText =
            if model.running then
                "Stop"
            else
                "Start"
    in
        input [ type_ "submit", value buttonText ] []



{- Make this:
   viewEntries : List Entry -> Html Msg
-}


viewEntries : Model -> Html Msg
viewEntries model =
    div [] [ ol [] (List.map viewEntry model.storedEntries) ]


viewEntry : Entry -> Html Msg
viewEntry entry =
    li []
        [ text
            ((viewTimeElapsed entry.startDate entry.endDate)
                ++ " - "
                ++ entry.description
            )
        ]

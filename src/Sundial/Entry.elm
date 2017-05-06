module Sundial.Entry
    exposing
        ( Entry(..)
        , EntryList
        , DateRangeEntryInfo
        , DurationDateEntryInfo
        , entryDuration
        , entryDescription
        )

import Date exposing (Date)
import Time exposing (Time)


type Entry
    = DateRangeEntry DateRangeEntryInfo
    | DurationDateEntry DurationDateEntryInfo


type alias DateRangeEntryInfo =
    { description : String
    , startDate : Date
    , endDate : Date
    }


type alias DurationDateEntryInfo =
    { description : String
    , duration : Time
    , date : Date
    }


entryDescription : Entry -> String
entryDescription entry =
    case entry of
        DateRangeEntry dateRangeEntryInfo ->
            dateRangeEntryInfo.description

        DurationDateEntry durationDateEntryInfo ->
            durationDateEntryInfo.description


entryDuration : Entry -> Time
entryDuration entry =
    case entry of
        DateRangeEntry dateRangeEntryInfo ->
            dateDiff dateRangeEntryInfo.startDate dateRangeEntryInfo.endDate

        DurationDateEntry durationDateEntryInfo ->
            durationDateEntryInfo.duration


dateDiff : Date -> Date -> Time
dateDiff startDate endDate =
    Date.toTime endDate - Date.toTime startDate

module DatePicker.Util exposing
    ( buildMonthMap
    , getDayInfo
    , getDayMonthText
    , getDayNum
    , getLastDayOfMonth
    , getMonthInfo
    , getMonthNumber
    , getNextMonthNumber
    , getPreviousMonthNumber
    , isJust
    , padMonthMap
    , setDayOfMonth
    )

import Date exposing (..)
import Time exposing (Month(..), Weekday(..))


getDayInfo : Weekday -> ( String, Int )
getDayInfo day =
    case day of
        Sun ->
            ( "Sun", 7 )

        Mon ->
            ( "Mon", 1 )

        Tue ->
            ( "Tue", 2 )

        Wed ->
            ( "Wed", 3 )

        Thu ->
            ( "Thu", 4 )

        Fri ->
            ( "Fri", 5 )

        Sat ->
            ( "Sat", 6 )


getDayNum : Date -> Int
getDayNum date =
    Tuple.second <| getDayInfo (weekday date)


getMonthInfo : Month -> ( String, Int )
getMonthInfo month =
    case month of
        Jan ->
            ( "January", 1 )

        Feb ->
            ( "Febuary", 2 )

        Mar ->
            ( "March", 3 )

        Apr ->
            ( "April", 4 )

        May ->
            ( "May", 5 )

        Jun ->
            ( "June", 6 )

        Jul ->
            ( "July", 7 )

        Aug ->
            ( "August", 8 )

        Sep ->
            ( "September", 9 )

        Oct ->
            ( "October", 10 )

        Nov ->
            ( "November", 11 )

        Dec ->
            ( "December", 12 )


getMonthNumber month =
    Tuple.second <| getMonthInfo month


getNextMonthNumber =
    getMonthNumber >> (+) 2


getPreviousMonthNumber =
    getMonthNumber >> (\n -> n - 1)


getLastDayOfMonth : Date -> Int -> Int
getLastDayOfMonth date prevTry =
    let
        nextDate =
            add Days 1 date
    in
    if Date.day nextDate > prevTry then
        getLastDayOfMonth nextDate (Date.day nextDate)

    else
        prevTry


placeholder =
    fromCalendarDate 1970 Jan 1


padMonthMap currentIndex stopIndex monthMap =
    if currentIndex == stopIndex then
        monthMap

    else
        padMonthMap (currentIndex + 1) stopIndex (( 0, placeholder ) :: monthMap)


buildMonthMap : List ( Int, Date ) -> Int -> Int -> Date -> Date -> List ( Int, Date )
buildMonthMap currentMap currentDay lastDay firstDate indexDate =
    let
        newMap =
            currentMap
                ++ [ ( currentDay
                     , fromCalendarDate
                        (Date.year indexDate)
                        (Date.month indexDate)
                        currentDay
                     )
                   ]
    in
    if lastDay /= currentDay then
        buildMonthMap newMap
            (currentDay + 1)
            lastDay
            firstDate
            indexDate

    else
        let
            padSize =
                weekdayNumber firstDate
        in
        padMonthMap 0 padSize newMap


setDayOfMonth : Date -> Int -> Date
setDayOfMonth date num =
    fromCalendarDate
        (Date.year date)
        (Date.month date)
        num


getDayMonthText date =
    let
        ( monthFull, monthInt ) =
            getMonthInfo <| Date.month date

        ( dayShort, dayInt ) =
            getDayInfo <| weekday date
    in
    dayShort ++ ", " ++ String.slice 0 3 monthFull ++ " " ++ (String.fromInt <| Date.day date)


isJust =
    Maybe.map (\_ -> True) >> Maybe.withDefault False

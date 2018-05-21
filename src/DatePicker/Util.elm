module DatePicker.Util exposing (..)

import Date exposing (..)
import Date.Extra.Duration exposing (add, Duration, Duration(..))
import Date.Extra.Create exposing (dateFromFields)


getDayInfo : Day -> ( String, Int )
getDayInfo day =
    case day of
        Sun ->
            ( "Sun", 1 )

        Mon ->
            ( "Mon", 2 )

        Tue ->
            ( "Tue", 3 )

        Wed ->
            ( "Wed", 4 )

        Thu ->
            ( "Thu", 5 )

        Fri ->
            ( "Fri", 6 )

        Sat ->
            ( "Sat", 7 )


getDayNum : Date -> Int
getDayNum date =
    Tuple.second <| getDayInfo (Date.dayOfWeek date)


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
    (getMonthNumber >> (+) 2)


getPreviousMonthNumber =
    (getMonthNumber >> (\n -> n - 1))


getLastDayOfMonth : Date -> Int -> Int
getLastDayOfMonth date prevTry =
    let
        nextDate =
            add Day 1 date
    in
        if (Date.day nextDate) > prevTry then
            getLastDayOfMonth nextDate (Date.day nextDate)
        else
            prevTry


padMonthMap currentIndex stopIndex monthMap =
    if currentIndex == stopIndex then
        monthMap
    else
        padMonthMap (currentIndex + 1) stopIndex (( 0, Date.fromTime 0 ) :: monthMap)


buildMonthMap : List ( Int, Date ) -> Int -> Int -> Date -> Date -> List ( Int, Date )
buildMonthMap currentMap currentDay lastDay firstDate indexDate =
    let
        newMap =
            (currentMap
                ++ [ ( currentDay
                     , dateFromFields
                        (Date.year indexDate)
                        (Date.month indexDate)
                        (currentDay)
                        0
                        0
                        0
                        0
                     )
                   ]
            )
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
                    (getDayNum firstDate) - 1
            in
                padMonthMap 0 padSize newMap


setDayOfMonth : Date -> Int -> Date
setDayOfMonth date num =
    dateFromFields
        (Date.year date)
        (Date.month date)
        num
        0
        0
        0
        0


getDayMonthText date =
    let
        ( monthFull, monthInt ) =
            getMonthInfo <| Date.month date

        ( dayShort, dayInt ) =
            getDayInfo <| Date.dayOfWeek date
    in
        dayShort ++ ", " ++ (String.slice 0 3 monthFull) ++ " " ++ (toString <| Date.day date)


isJust =
    (Maybe.map (\_ -> True) >> Maybe.withDefault False)

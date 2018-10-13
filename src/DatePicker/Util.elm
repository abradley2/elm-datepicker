module DatePicker.Util exposing
    ( buildMonthMap
    , daySymbol
    , getDayInfo
    , getDayMonthText
    , getDayNum
    , getLastDayOfMonth
    , getMonthInfo
    , isJust
    , monthDisplay
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


getMonthInfo : Month -> (Month -> String) -> ( String, Int )
getMonthInfo month toStr =
    case month of
        Jan ->
            ( toStr Jan, 1 )

        Feb ->
            ( toStr Feb, 2 )

        Mar ->
            ( toStr Mar, 3 )

        Apr ->
            ( toStr Apr, 4 )

        May ->
            ( toStr May, 5 )

        Jun ->
            ( toStr Jun, 6 )

        Jul ->
            ( toStr Jul, 7 )

        Aug ->
            ( toStr Aug, 8 )

        Sep ->
            ( toStr Sep, 9 )

        Oct ->
            ( toStr Oct, 10 )

        Nov ->
            ( toStr Nov, 11 )

        Dec ->
            ( toStr Dec, 12 )


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
            getMonthInfo (Date.month date) monthDisplay

        ( dayShort, dayInt ) =
            getDayInfo <| weekday date
    in
    dayShort ++ ", " ++ String.slice 0 3 monthFull ++ " " ++ (String.fromInt <| Date.day date)


isJust =
    Maybe.map (\_ -> True) >> Maybe.withDefault False


monthDisplay : Month -> String
monthDisplay month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "Aug"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


daySymbol : Weekday -> String
daySymbol weekday =
    case weekday of
        Mon ->
            "M"

        Tue ->
            "T"

        Wed ->
            "W"

        Thu ->
            "T"

        Fri ->
            "F"

        Sat ->
            "S"

        Sun ->
            "S"

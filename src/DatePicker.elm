module DatePicker exposing
    ( Msg(..), init, update, Model
    , view, Props, defaultProps
    , setIndexDate
    , SelectionMode
    )

{-| This module provides a styled date picker for Elm.
[You can check out the demo here.](http://abradley2.github.io/elm-datepicker/)

To alter the color theme edit `./styl/Variables.styl`, then run
`npm install && npm run build-styles`.

This library depends heavily on [justinmimbs/date](https://package.elm-lang.org/packages/justinmimbs/date/3.1.2/) as
this provides a nice RD based wrapper around Dates that is agnostic to time/timezone, which is better suited for
calendars. See the documentation there for any specific handling of the `Date` type.


# Tea / Initialization

@docs Msg, init, update, Model


# Rendering and Settings

@docs view, Props, defaultProps


# Helpers

@docs setIndexDate


# Types

@docs SelectionMode

-}

import Browser.Dom as Dom
import Date exposing (..)
import DatePicker.Util exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Task
import Time exposing (Month(..), Weekday(..))


{-| Represents the current mode the picker is set to

    type SelectionMode
        = Calendar
        | YearPicker

-}
type SelectionMode
    = Calendar
    | YearPicker


type MonthChange
    = None
    | Previous
    | Next


{-| You will first need to add the `DatePicker.Msg` to the type consumed by your `update` function so
it recognizes this type.

    import DatePicker
    ...
    type Msg
        = FireZeMissiles
        | DatePickerMsg DatePicker.Msg

-}
type Msg
    = NoOp
    | DateSelected Date Date
    | GetToday Date
    | PreviousMonth Date
    | NextMonth Date
    | SubmitClicked Date
    | CancelClicked
    | SetSelectionMode SelectionMode


{-| The `DatePicker.Model` type needs to be added to any data structure that requires a picker instance

    import DatePicker
    ...
    type alias Model =
        { datePickerData : DatePicker.Model
        }
    }

This is mostly an opaque type you don't have to worry about, though there are some important fields you will
want to use:

  - `today` is the default "selected" day of the picker before the user has actually clicked to "select" a day.
    This is needed so the head display isn't empty before the user has selected anything, without forcing there be a default selected date of "today".
  - `indexDate` is a date used to track which month the calendar is currently showing. Do not set this directly. Use the `setIndexDate` helper
  - `selectedDate` is the last date the user clicked on in the calendar that was selectable
  - `selectionMode` determines whether the user sees the `Calendar` or the `YearPicker`

-}
type alias Model =
    { id : String
    , today : Maybe Date
    , indexDate : Maybe Date
    , currentMonthMap : Maybe (List ( Int, Date ))
    , previousMonthMap : Maybe (List ( Int, Date ))
    , selectedDate : Maybe Date
    , previousSelectedDate : Maybe Date
    , monthChange : MonthChange
    , selectionMode : SelectionMode
    , yearList : Maybe (List Int)
    }


type alias InitializedModel =
    { id : String
    , today : Date
    , indexDate : Date
    , currentMonthMap : List ( Int, Date )
    , previousMonthMap : Maybe (List ( Int, Date ))
    , selectedDate : Maybe Date
    , previousSelectedDate : Maybe Date
    , monthChange : MonthChange
    , selectionMode : SelectionMode
    , yearList : List Int
    }


{-| Takes any of type `DatePicker.Model` and returns a new one with the given index date. It is
important to not just set indexDate directly as this will not refresh the data to completely
reflect this
-}
setIndexDate : Model -> Date -> Model
setIndexDate model indexDate =
    let
        lastDayOfMonth =
            getLastDayOfMonth indexDate (Date.day indexDate)

        monthMap =
            buildMonthMap
                []
                1
                lastDayOfMonth
                (setDayOfMonth indexDate 1)
                indexDate
    in
    { model
        | indexDate =
            -- this will set it to beginning of day
            Just (setDayOfMonth indexDate (Date.day indexDate))
        , currentMonthMap = Just monthMap

        -- only set previous month map if index date is of different month
        , previousMonthMap =
            Maybe.map
                (\prevIndexDate ->
                    if isNewMonth indexDate prevIndexDate then
                        model.currentMonthMap

                    else
                        Nothing
                )
                model.indexDate
                |> Maybe.withDefault Nothing
    }


isNewMonth : Date -> Date -> Bool
isNewMonth a b =
    Date.month a /= Date.month b || Date.year a /= Date.year b


{-| `DatePicker.init` returns an initialized record of `DatePicker.Model`. Do not throw out the returned command!
The command is used to get today's current date which the date picker uses as the default for display.
The string passed as the first argument must be a unique `id` for the date picker.

    import DatePicker

    init : ( Model, Cmd Msg )
    init =
        let
            ( datePickerData, datePickerInitCmd ) =
                DatePicker.init "my-datepicker-id"
        in
        ( { datePickerData = datePickerData
          , selectedDate = Nothing
          }
        , Cmd.map DatePickerMsg datePickerInitCmd
        )

-}
init : String -> ( Model, Cmd Msg )
init id =
    ( { id = id
      , today = Nothing
      , indexDate = Nothing
      , currentMonthMap = Nothing
      , previousMonthMap = Nothing
      , selectedDate = Nothing
      , previousSelectedDate = Nothing
      , monthChange = None
      , selectionMode = Calendar
      , yearList = Nothing
      }
    , Task.perform GetToday Date.today
    )


{-| Use `DatePicker.update` to create updated date picker models from any message events.
For a nice full working example check out the [demo source here](https://github.com/abradley2/elm-datepicker/blob/master/src/Demo.elm)

    import DatePicker exposing (Msg(..))
    ...
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            NoOp ->
                ( model, Cmd.none )

        DatePickerMsg datePickerMsg ->
            let
                (updatedPicker, pickerCmd) =
                    DatePicker.update
                        datepickerMsg
                        model.datePickerData
            in
                ({ model
                 | datePickerData = updatedPicker
                 , selectedDate = datePickerData.selectedDate
                }
                , Cmd.map DatePickerMsg pickerCmd
                )

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DateSelected date previousDate ->
            let
                newModel =
                    setIndexDate model date
            in
            ( { newModel
                | selectedDate = Just date
                , previousSelectedDate = Just previousDate
                , selectionMode = Calendar
              }
            , Cmd.none
            )

        NextMonth newIndexDate ->
            let
                newModel =
                    setIndexDate model newIndexDate
            in
            ( { newModel
                | monthChange = Next
              }
            , Cmd.none
            )

        PreviousMonth newIndexDate ->
            let
                newModel =
                    setIndexDate model newIndexDate
            in
            ( { newModel
                | monthChange = Previous
              }
            , Cmd.none
            )

        GetToday today ->
            let
                updatedModel =
                    setIndexDate model today
            in
            ( { updatedModel
                | today = Just (setDayOfMonth today (Date.day today))
                , yearList = Just (defaultedYearList model.yearList today)
              }
            , Cmd.none
            )

        SetSelectionMode mode ->
            case ( mode, model.today, model.yearList ) of
                ( YearPicker, Just today, Just yearList ) ->
                    let
                        workingDate =
                            Maybe.withDefault today model.selectedDate

                        scrollId =
                            "edp-year-picker-" ++ model.id

                        selectedYearIndex =
                            List.partition (\year -> year <= Date.year workingDate) yearList
                                |> Tuple.first
                                |> List.length

                        yOffset =
                            (toFloat selectedYearIndex * 40) - (4 * 40)
                    in
                    ( { model | selectionMode = mode }
                    , Task.attempt (\_ -> NoOp) (Dom.setViewportOf scrollId 0 yOffset)
                    )

                ( Calendar, _, _ ) ->
                    ( { model | selectionMode = mode, monthChange = Next }, Cmd.none )

                ( _, _, _ ) ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| The second argument passed to `DatePicker.view`. These are configuration properties
that generally determine the range of selectable dates. Extend off `DatePicker.defaultProps`
to avoid having to define all of these when you only wish to configure a few.

---

**Property Descriptions**

given the year, return whether this year is allowed to be selected

    canSelectYear : Int -> Bool

given the year and the month, return whether this month is allowed to be selected

    canSelectMonth : Int -> Month -> Bool

given the date, return whether this is allowed to be selected

    canSelectDate : Date -> Bool

should the footer of the calendar with the "CANCEL" and "OK" buttons display

    hideFooter : Bool

text for the "OK" button which is enabled whenever a date is selected.
defaults to "OK"

    okButtonText : String

text for the "CANCEL" button. Defaults to "CANCEL"

    cancelButtonText : String

return whatever text to show given the month (this is just below the calendar header)

    monthDisplay : Time.Month -> String

return whatever text (generally a single letter or two) to show
given the weekday (these are the small letters the top of the month)

    daySymbol : Time.Weekday -> String

return whatever text to show given the selected Date
(This is the large display text on the calendar header)
The first date is the "selectedDate" which may not yet be defined.
The second is the "indexDate" which is the current placeholder
date being used (generally set to today's date by default)

    selectedDateDisplay : Maybe Date -> Date -> String

-}
type alias Props =
    { canSelectYear : Int -> Bool
    , canSelectMonth : Int -> Month -> Bool
    , canSelectDate : Date -> Bool
    , hideFooter : Bool
    , monthDisplay : Month -> String
    , daySymbol : Weekday -> String
    , selectedDateDisplay : Maybe Date -> Date -> String
    , okButtonText : String
    , cancelButtonText : String
    }


defaultedYearList : Maybe (List Int) -> Date -> List Int
defaultedYearList yearList indexDate =
    case yearList of
        Just list ->
            list

        Nothing ->
            List.range (Date.year indexDate - 120) (Date.year indexDate + 120)


{-| Use the default props if you don't want to support any sort of configuration.
These mostly center around limiting the user to a specific selection range of dates.
By default, nothing is restricted.

Here's an example of how you might configure these:

    getDatePickerProps : DatePicker.Props
    getDatePickerProps =
        let
            defaultProps =
                DatePicker.defaultProps
        in
        { defaultProps
            | canSelectYear = \year -> year < 2020
            , okButtonText = "CONFIRM"
        }

-}
defaultProps : Props
defaultProps =
    { canSelectYear = \year -> True
    , canSelectMonth = \year month -> True
    , canSelectDate = \date -> True
    , hideFooter = False
    , monthDisplay = monthDisplay
    , daySymbol = daySymbol
    , selectedDateDisplay =
        \maybeDate date ->
            Maybe.map getDayMonthText maybeDate
                |> Maybe.withDefault (getDayMonthText date)
    , okButtonText = "OK"
    , cancelButtonText = "CANCEL"
    }


displayYear =
    Date.year >> String.fromInt >> text


headerYearDisplay : Date -> InitializedModel -> Props -> Html Msg
headerYearDisplay displayDate model props =
    div
        [ classList
            [ ( "edp-header-year", True )
            , ( "edp-header-active", model.selectionMode == YearPicker )
            ]
        , onClick (SetSelectionMode YearPicker)
        ]
        [ displayYear displayDate ]


headerDayMonthDisplay : Bool -> Maybe Date -> InitializedModel -> Props -> Maybe ( String, Html Msg )
headerDayMonthDisplay isPreviousDate date model props =
    Maybe.map
        (\justDate ->
            ( props.selectedDateDisplay date model.indexDate
            , div
                [ classList
                    [ ( "edp-header-month-day", True )
                    , ( "edp-header-active", model.selectionMode == Calendar )
                    , ( "edp-month-day-previous", isPreviousDate )
                    ]
                , onClick <| SetSelectionMode Calendar
                ]
                [ text (props.selectedDateDisplay date model.indexDate)
                ]
            )
        )
        date


headerSection : Date -> InitializedModel -> Props -> Html Msg
headerSection displayDate model props =
    div
        [ class "edp-header-section"
        ]
        [ headerYearDisplay displayDate model props
        , Keyed.node "div"
            [ class "edp-month-day-wrapper"
            ]
            [ Maybe.withDefault
                ( "previous", div [] [] )
                (headerDayMonthDisplay True
                    model.previousSelectedDate
                    model
                    props
                )
            , Maybe.withDefault
                ( "current", div [] [] )
                (headerDayMonthDisplay False
                    (if isJust model.selectedDate then
                        model.selectedDate

                     else
                        Just model.today
                    )
                    model
                    props
                )
            ]
        ]


monthChangeSection : InitializedModel -> Props -> Html Msg
monthChangeSection model props =
    let
        year =
            Date.year model.indexDate

        previousMonthIndexDate =
            add Months -1 model.indexDate

        nextMonthIndexDate =
            add Months 1 model.indexDate

        canSelectNext =
            props.canSelectMonth year (Date.month nextMonthIndexDate)
                && props.canSelectYear (Date.year nextMonthIndexDate)

        canSelectPrevious =
            props.canSelectMonth year (Date.month previousMonthIndexDate)
                && props.canSelectYear (Date.year previousMonthIndexDate)
    in
    div
        [ class "edp-month-change-section edp-body-section"
        ]
        [ div
            [ classList
                [ ( "edp-caret-button", True )
                , ( "edp-disabled", canSelectPrevious == False )
                ]
            , onClick <|
                if canSelectPrevious then
                    PreviousMonth previousMonthIndexDate

                else
                    NoOp
            ]
            [ div
                [ classList
                    [ ( "edp-caret edp-caret-left", True )
                    , ( "edp-disabled", not canSelectPrevious )
                    ]
                ]
                []
            ]
        , div []
            [ text
                (let
                    ( monthFull, monthInt ) =
                        getMonthInfo (Date.month model.indexDate) props.monthDisplay
                 in
                 monthFull ++ " " ++ (String.fromInt <| Date.year model.indexDate)
                )
            ]
        , div
            [ classList
                [ ( "edp-caret-button", True )
                , ( "edp-disabled", canSelectNext == False )
                ]
            , onClick <|
                if canSelectNext then
                    NextMonth nextMonthIndexDate

                else
                    NoOp
            ]
            [ div
                [ classList
                    [ ( "edp-caret edp-caret-right", True )
                    , ( "edp-disabled", not canSelectNext )
                    ]
                ]
                []
            ]
        ]


weekSection : InitializedModel -> Props -> Html Msg
weekSection model props =
    div [ class "edp-body-section" ]
        (List.map
            (\symbol ->
                div [ class "edp-column edp-day-symbol" ] [ text symbol ]
            )
            (List.map props.daySymbol
                [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]
            )
        )


daySectionMonth : InitializedModel -> Props -> Html Msg
daySectionMonth model props =
    div [ class "edp-body-section" ]
        (List.map
            (\( dayNum, date ) ->
                let
                    isSelected =
                        case model.selectedDate of
                            Just selected ->
                                Date.toRataDie selected == Date.toRataDie date

                            Nothing ->
                                False

                    isToday =
                        Date.toRataDie model.today == Date.toRataDie date

                    isPlaceholder =
                        dayNum == 0

                    canSelect =
                        not isPlaceholder && props.canSelectDate date
                in
                div
                    [ classList
                        [ ( "edp-column edp-day-number", True )
                        , ( "edp-empty-column", dayNum == 0 )
                        , ( "edp-disabled-column", not isPlaceholder && canSelect == False )
                        , ( "edp-day-number-selected", isSelected )
                        , ( "edp-day-number-today", isToday )
                        ]
                    , onClick
                        (case ( canSelect, model.selectedDate ) of
                            ( True, Just previousSelected ) ->
                                if Date.toRataDie previousSelected == Date.toRataDie date then
                                    NoOp

                                else
                                    DateSelected date previousSelected

                            ( True, Nothing ) ->
                                DateSelected date model.today

                            ( False, _ ) ->
                                NoOp
                        )
                    ]
                    [ text <|
                        if isPlaceholder then
                            "."

                        else
                            String.fromInt dayNum
                    ]
            )
            model.currentMonthMap
        )


getMonthKey : Date -> String
getMonthKey date =
    Tuple.first <| getMonthInfo (Date.month date) monthDisplay


previousMonthBody : InitializedModel -> Props -> Maybe ( String, Html Msg )
previousMonthBody model props =
    Maybe.map
        (\previousMonthMap ->
            ( getMonthKey model.indexDate ++ "-previous"
            , div
                [ classList
                    [ ( "edp-month-slider", True )
                    , ( "edp-out-next", model.monthChange == Next )
                    , ( "edp-out-previous", model.monthChange == Previous )
                    ]
                ]
                [ daySectionMonth { model | currentMonthMap = previousMonthMap } props
                ]
            )
        )
        model.previousMonthMap


calendarBody : InitializedModel -> Props -> Html Msg
calendarBody model props =
    Keyed.node "div"
        [ class "edp-month-wrapper"
        ]
        [ Maybe.withDefault ( "only", div [] [] ) (previousMonthBody model props)
        , ( getMonthKey model.indexDate
          , div
                [ classList
                    [ ( "edp-month-slider", True )
                    , ( "edp-in-next", model.monthChange == Next )
                    , ( "edp-in-previous", model.monthChange == Previous )
                    ]
                ]
                [ daySectionMonth model props
                ]
          )
        ]


bottomSection : InitializedModel -> Props -> Html Msg
bottomSection model props =
    let
        disableOk =
            model.selectedDate == Nothing
    in
    div [ class "edp-body-section edp-bottom-section" ]
        [ button
            [ onClick CancelClicked
            , class "edp-button"
            ]
            [ text props.cancelButtonText ]
        , button
            [ classList
                [ ( "edp-button", True )
                , ( "edp-disabled", model.selectedDate == Nothing )
                ]
            , onClick
                (case model.selectedDate of
                    Just date ->
                        SubmitClicked date

                    Nothing ->
                        NoOp
                )
            ]
            [ text props.okButtonText ]
        ]


yearSection : InitializedModel -> Props -> Html Msg
yearSection model props =
    let
        workingDate =
            Maybe.withDefault model.today model.selectedDate

        applyYear dt year =
            Date.fromCalendarDate
                year
                (Date.month dt)
                (Date.day dt)

        viewYear year =
            let
                canSelect =
                    props.canSelectYear year
            in
            div []
                [ button
                    [ classList
                        [ ( "edp-button", True )
                        , ( "edp-year-button", True )
                        , ( "edp-year-button-selected", year == Date.year workingDate )
                        , ( "edp-disabled", not canSelect )
                        ]
                    , onClick <|
                        if canSelect then
                            DateSelected (applyYear workingDate year) workingDate

                        else
                            NoOp
                    ]
                    [ text (String.fromInt year) ]
                ]
    in
    div [ id <| "edp-year-picker-" ++ model.id, class "edp-year-picker" ] [ div [ class "edp-year-picker-body" ] (List.map viewYear model.yearList) ]


{-| The main view for the date picker. Use `Html.map` so the returned type doesn't conflict with
your view's type.

    import DatePicker
    ...
    view : Model -> Html Msg
    view model =
        Html.map DatePickerMsg <|
            DatePicker.view
                model.datePickerData
                DatePicker.defaultProps

-}
view : Model -> Props -> Html Msg
view model props =
    Maybe.withDefault (div [ class "edp-container" ] []) <|
        Maybe.map4
            (\today indexDate currentMonthMap yearList ->
                let
                    displayDate =
                        Maybe.withDefault today model.selectedDate

                    initializedModel =
                        { id = model.id
                        , today = today
                        , indexDate = indexDate
                        , selectedDate = model.selectedDate
                        , previousSelectedDate = model.previousSelectedDate
                        , currentMonthMap = currentMonthMap
                        , previousMonthMap = model.previousMonthMap
                        , monthChange = model.monthChange
                        , selectionMode = model.selectionMode
                        , yearList = yearList
                        }

                    footer =
                        if props.hideFooter then
                            div [] []

                        else
                            bottomSection initializedModel props

                    mainSection =
                        case initializedModel.selectionMode of
                            Calendar ->
                                div []
                                    [ monthChangeSection initializedModel props
                                    , weekSection initializedModel props
                                    , calendarBody initializedModel props
                                    , footer
                                    ]

                            YearPicker ->
                                div []
                                    [ yearSection initializedModel props
                                    , footer
                                    ]
                in
                div
                    [ class "edp-container"
                    ]
                    [ headerSection displayDate initializedModel props
                    , mainSection
                    ]
            )
            model.today
            model.indexDate
            model.currentMonthMap
            model.yearList

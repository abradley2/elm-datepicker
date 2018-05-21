module DatePicker
    exposing
        ( view
        , init
        , defaultProps
        , update
        , SelectionMode
        , Props
        , Model
        , Msg
        , Msg(..)
        , setIndexDate
        )

{-| This module provides a material-style date picker for Elm.
[You can check out the demo here.](http://abradley2.github.io/elm-datepicker/)
Since this date picker
uses material-icons, you will need to have these included on your page.
Also include the `DatePicker.css` found in the root of this directory.


    <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
    <link rel="stylesheet" type="text/css" href="DatePicker.css" />


To alter the color theme edit `./styl/Variables.styl`, then run
`npm install && npm run build-styles`.


# Tea / Initialization
@docs Msg, init, update, Model

# Rendering and Settings
@docs view, Props, defaultProps

# Helpers
@docs setIndexDate

# Types
@docs SelectionMode
-}

import Process
import Task
import Dict
import Dom
import Dom.Scroll exposing (toY)
import Date exposing (..)
import Date.Extra.Duration exposing (Duration, Duration(..), add)
import Date.Extra.Create exposing (dateFromFields)
import Html exposing (..)
import Html.Keyed as Keyed
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import DatePicker.Util exposing (..)


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
    | SetYear Date Int
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

* `today` is the default "selected" day of the picker before the user has actually clicked to "select" a day.
This is needed so the head display isn't empty before the user has selected anything, without forcing there be a default selected date of "today".
* `indexDate` is a date used to track which month the calendar is currently showing. Do not set this directly. Use the `setIndexDate` helper
* `selectedDate` is the last date the user clicked on in the calendar that was selectable
* `selectionMode` determines whether the user sees the `Calendar` or the `YearPicker`
-}
type alias Model =
    { id : String
    , today : Maybe Date
    , indexDate : Maybe Date
    , currentMonthMap : Maybe (List ( Int, Date ))
    , previousMonthMap : Maybe (List ( Int, Date ))
    , selectedDate : Maybe Date
    , previousSelectedDate : Maybe Date
    , selectionMode : SelectionMode
    , monthChange : MonthChange
    , yearList : List Int
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
    , yearList : List Int
    , selectionMode : SelectionMode
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
                (lastDayOfMonth)
                (setDayOfMonth indexDate 1)
                indexDate

        yearList =
            List.range (Date.year indexDate - 120) (Date.year indexDate + 120)
    in
        { model
            | indexDate =
                -- this will set it to beginning of day
                Just (setDayOfMonth indexDate (Date.day indexDate))
            , currentMonthMap = Just monthMap
            , previousMonthMap = model.currentMonthMap
            , yearList = yearList
        }


{-| `DatePicker.init` returns an initialized record of `DatePicker.Model`. Do not throw out the returned command!
The command is used to get today's current date which the date picker uses as the default for display.
The string passed as the first argument must be a unique `id` for the date picker

    import DatePicker

    init : (Model, Cmd Msg)
    init =
        let
            (datePickerData, datePickerInitCmd) =
                DatePicker.init "my-datepicker-id"
        in
            ({ datePickerData = datePickerData
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
      , selectionMode = Calendar
      , monthChange = None
      , yearList = []
      }
    , Task.perform GetToday Date.now
    )


{-| `DatePicker.update` consumes the message you've mapped and a `DatePicker.Model` record to output `( DatePicker.Model, Cmd DatePicker.Msg)`.
You will need to alter your update function to handle any `DatePicker.Msg` that flows through.


    import DatePicker exposing (Msg(SelectDate))
    ...
    handleDatePickerMsg model datePickerMsg =
        let
            (datePickerData, datePickerCmd) =
                DatePicker.update datePickerMsg model.datePickerData
        in
            ({ model
             | datePickerData = datePickerData
             }
            , Cmd.map DatePickerMsg datePickerCmd
            )
    ...
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            NoOp ->
                ( model, Cmd.none )

            DatePickerMsg datePickerMsg ->
                handleDatePickerMsg model datePickerMsg
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DateSelected date previousDate ->
            ( { model
                | selectedDate = Just date
                , previousSelectedDate = Just previousDate
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

        SetYear indexDate newYear ->
            let
                newModel =
                    setIndexDate model (dateFromFields newYear (Date.month indexDate) 1 0 0 0 0)
            in
                ( { newModel
                    | selectionMode = Calendar
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
                  }
                , Cmd.none
                )

        SetSelectionMode selectionMode ->
            ( { model
                | selectionMode = selectionMode
                , previousMonthMap =
                    Nothing
                    -- so we don't animate to next month when this div remounts
              }
            , case selectionMode of
                Calendar ->
                    Cmd.none

                YearPicker ->
                    toY ("edp-year-picker-scroll-" ++ model.id) (List.length model.yearList |> toFloat |> \l -> l * 40 / 2 - 120)
                        |> Task.attempt (always NoOp)
            )

        _ ->
            ( model, Cmd.none )


{-| The second argument passed to `DatePicker.view`. These are configuration properties
that generally determine the range of selectable dates
-}
type alias Props =
    { canSelectYear : Int -> Bool
    , canSelectMonth : Int -> Int -> Bool
    , canSelectDate : Date -> Bool
    , hideFooter : Bool
    }


{-| Use the default props if you don't want to support any sort of configuration.
These mostly center around limiting the user to a specific selection range of dates.
By default, nothing is restricted.

    defaultProps =
        { canSelectYear = \year -> True
        , canSelectMonth = \year month -> True
        , canSelectDate = \date -> True
        , hideFooter = False
        }
-}
defaultProps : Props
defaultProps =
    { canSelectYear = \year -> True
    , canSelectMonth = \year month -> True
    , canSelectDate = \date -> True
    , hideFooter = True
    }


headerSection : InitializedModel -> Props -> Html Msg
headerSection model props =
    let
        ( yearText, dayMonthText ) =
            case model.selectedDate of
                Just selected ->
                    ( toString <| Date.year selected
                    , getDayMonthText selected
                    )

                Nothing ->
                    ( toString <| Date.year model.today, getDayMonthText model.today )
    in
        div
            [ class "edp-header-section"
            ]
            [ div
                [ classList
                    [ ( "edp-header-year", True )
                    , ( "edp-header-active", Debug.log "year is active " <| model.selectionMode == YearPicker )
                    ]
                , onClick (SetSelectionMode YearPicker)
                ]
                [ text yearText ]
            , Keyed.node "div"
                [ class "edp-month-day-wrapper"
                ]
                ([ ( dayMonthText
                   , div
                        [ classList
                            [ ( "edp-header-month-day", True )
                            , ( "edp-header-active", model.selectionMode == Calendar )
                            , ( "edp-month-day-next", True )
                            ]
                        , onClick (SetSelectionMode Calendar)
                        ]
                        [ text dayMonthText
                        ]
                   )
                 ]
                    -- if we have a previous date we need to render this in so we have an animation
                    ++
                        (case model.previousSelectedDate of
                            Just previousDate ->
                                [ ( getDayMonthText previousDate
                                  , div
                                        [ classList
                                            [ ( "edp-header-month-day", True )
                                            , ( "edp-header-active", model.selectionMode == Calendar )
                                            , ( "edp-month-day-previous", True )
                                            ]
                                        , onClick (SetSelectionMode Calendar)
                                        ]
                                        [ text (getDayMonthText previousDate)
                                        ]
                                  )
                                ]

                            Nothing ->
                                [ ( "empty", div [] [] ) ]
                        )
                )
            ]


monthChangeSection : InitializedModel -> Props -> Html Msg
monthChangeSection model props =
    let
        year =
            Date.year model.indexDate

        month =
            Date.month model.indexDate

        canSelectNext =
            props.canSelectMonth year <| getNextMonthNumber month

        canSelectPrevious =
            props.canSelectMonth year <| getPreviousMonthNumber month
    in
        div
            [ class "edp-month-change-section edp-body-section"
            ]
            [ div
                [ onClick <|
                    if canSelectPrevious then
                        (PreviousMonth (add Month -1 model.indexDate))
                    else
                        NoOp
                ]
                [ i
                    [ classList
                        [ ( "material-icons arrow-icon", True )
                        , ( "edp-disabled", canSelectPrevious == False )
                        ]
                    ]
                    [ text "keyboard_arrow_left" ]
                ]
            , div []
                [ text
                    (let
                        ( monthFull, monthInt ) =
                            getMonthInfo (Date.month model.indexDate)
                     in
                        monthFull ++ " " ++ (toString <| Date.year model.indexDate)
                    )
                ]
            , div
                [ onClick <|
                    if canSelectNext then
                        (NextMonth (add Month 1 model.indexDate))
                    else
                        NoOp
                ]
                [ i
                    [ classList
                        [ ( "material-icons arrow-icon", True )
                        , ( "edp-disabled", canSelectPrevious == False )
                        ]
                    ]
                    [ text "keyboard_arrow_right" ]
                ]
            ]


weekSection : InitializedModel -> Props -> Html Msg
weekSection model props =
    div [ class "edp-body-section" ]
        (List.map
            (\symbol ->
                div [ class "edp-column edp-day-symbol" ] [ text symbol ]
            )
            [ "S", "M", "T", "W", "T", "F", "S" ]
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
                                Date.toTime selected == Date.toTime date

                            Nothing ->
                                False

                    isToday =
                        Date.toTime model.today == Date.toTime date

                    canSelect =
                        props.canSelectDate date
                in
                    div
                        [ classList
                            [ ( "edp-column edp-day-number", True )
                            , ( "edp-empty-column", dayNum == 0 )
                            , ( "edp-disabled-column", canSelect == False )
                            , ( "edp-day-number-selected", isSelected )
                            , ( "edp-day-number-today", isToday )
                            ]
                        , onClick
                            (case model.selectedDate of
                                Just previousSelected ->
                                    if Date.toTime previousSelected == Date.toTime date then
                                        NoOp
                                    else
                                        DateSelected date previousSelected

                                Nothing ->
                                    DateSelected date model.today
                            )
                        ]
                        [ text
                            (toString dayNum)
                        ]
            )
            model.currentMonthMap
        )


getMonthKey : Date -> String
getMonthKey date =
    toString (Tuple.first <| getMonthInfo (Date.month date))


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
                [ text "CANCEL" ]
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
                [ text "OK" ]
            ]


yearPickerSection : InitializedModel -> Props -> Html Msg
yearPickerSection model props =
    div
        [ classList
            [ ( "edp-year-picker", True )
            ]
        , id <| "edp-year-picker-scroll-" ++ model.id
        ]
        [ let
            listHeight =
                toString <| (List.length model.yearList) * 40

            offset =
                toString <| (List.length model.yearList) * 40 // 2 - 40

            selectedYear =
                Date.year <| Maybe.withDefault model.today model.selectedDate
          in
            div
                [ classList
                    [ ( "edp-year-picker-body", True )
                    ]
                , style
                    [ ( "height", listHeight ++ "px" )
                    ]
                ]
                (List.map
                    (\year ->
                        div
                            []
                            [ button
                                [ classList
                                    [ ( "edp-button", True )
                                    , ( "edp-year-button", True )
                                    , ( "edp-year-button-selected", year == selectedYear )
                                    ]
                                , onClick (SetYear model.indexDate year)
                                ]
                                [ text (toString year) ]
                            ]
                    )
                    model.yearList
                )
        ]


{-|
The main view for the date picker. Use `Html.map` so the returned type doesn't conflict with
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
        Maybe.map3
            (\today indexDate currentMonthMap ->
                let
                    initializedModel =
                        { id = model.id
                        , today = today
                        , indexDate = indexDate
                        , selectedDate = model.selectedDate
                        , previousSelectedDate = model.previousSelectedDate
                        , currentMonthMap = currentMonthMap
                        , previousMonthMap = model.previousMonthMap
                        , monthChange = model.monthChange
                        , yearList = model.yearList
                        , selectionMode = model.selectionMode
                        }

                    footer =
                        if props.hideFooter then
                            div [] []
                        else
                            bottomSection initializedModel props
                in
                    div
                        [ class "edp-container"
                        ]
                        [ headerSection initializedModel props
                        , div []
                            (case model.selectionMode of
                                Calendar ->
                                    [ monthChangeSection initializedModel props
                                    , weekSection initializedModel props
                                    , calendarBody initializedModel props
                                    , footer
                                    ]

                                YearPicker ->
                                    [ yearPickerSection initializedModel props
                                    , footer
                                    ]
                            )
                        ]
            )
            model.today
            model.indexDate
            model.currentMonthMap

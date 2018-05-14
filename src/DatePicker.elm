module DatePicker
    exposing
        ( datePickerView
        , datePickerInit
        , setIndexDate
        , datePickerUpdate
        , SelectionMode
        , DatePickerProps
        , DatePickerModel
        , DatePickerMsg
        , DatePickerMsg(..)
        , setDayOfMonth
        )

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Tea / Initialization
@docs DatePickerMsg, datePickerInit, datePickerUpdate, DatePickerModel

# Rendering and Settings
@docs datePickerView, DatePickerProps

# Helpers
@docs setIndexDate, setDayOfMonth

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
-}
type SelectionMode
    = Calendar
    | YearPicker


type MonthChange
    = Previous
    | Next


{-| You will first need to add the `DatePickerMsg` to the type consumed by your `update` function so
it recognizes this type.
    import DatePicker exposing (DatePickerMsg, DatePickerMsg(..))

    type Msg
        = FireZeMissiles
        | HandleDatePickerMsg DatePickerMsg
-}
type DatePickerMsg
    = NoOp
    | DateSelected Date Date
    | GetToday Date
    | SetYear Date Int
    | PreviousMonth Date
    | NextMonth Date
    | SubmitClicked Date
    | CancelClicked
    | SetSelectionMode SelectionMode


{-| The DatePickerModel type needs to be added to any data structure that requires a picker instance
    import DatePicker exposing (DatePickerModel)

    type alias Model =
        { selectedDate : Maybe Date
        , datePickerData : DatePickerModel
        }
    }

    init : Model
    ...

This is mostly an opaque type you don't have to worry about, with the exception of the `selectedDate` field,
which tells you what date the picker has currently selected- something you may not care about if you only want
the date when you recieve the `SubmitClicked` message.
-}
type alias DatePickerModel =
    { id : String
    , today : Maybe Date
    , indexDate : Maybe Date
    , currentMonthMap : Maybe (List ( Int, Date ))
    , previousMonthMap : Maybe (List ( Int, Date ))
    , selectedDate : Maybe Date
    , previousSelectedDate : Maybe Date
    , colors : Dict.Dict String String
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
    , colors : Dict.Dict String String
    , monthChange : MonthChange
    , yearList : List Int
    , selectionMode : SelectionMode
    }


getColor : InitializedModel -> String -> String
getColor model color =
    Maybe.withDefault "red" (Dict.get color model.colors)


{-| Given a date and a number reflecting the day of that month, returns a new date with the day to to that input
-}
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


{-| Takes an instance of DatePickerModel and returns a new one with the given date. It is
important to not just set indexDate directly as this will not refresh the data to completely
reflect this
-}
setIndexDate : DatePickerModel -> Date -> DatePickerModel
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
            if List.length model.yearList == 0 then
                List.range (Date.year indexDate - 120) (Date.year indexDate + 120)
            else
                model.yearList
    in
        { model
            | indexDate =
                -- this will set it to beginning of day
                Just (setDayOfMonth indexDate (Date.day indexDate))
            , currentMonthMap = Just monthMap
            , previousMonthMap = model.currentMonthMap
            , yearList = yearList
        }


{-| datePickerInit returns an initialized DatePickerModel and a `Cmd` to initialize the picker

    import DatePicker exposing (datePickerInit)

    init : (Model, Cmd Msg)
    init =
        let
            (datePickerData, datePickerInitCmd) =
                datePickerInit "my-datepicker-id"
        in
            ({ datePickerData = datePickerData
             , selectedDate = Nothing
             }
             , Cmd.map SetDatePickerMsg datePickerInitCmd
            )
-}
datePickerInit : String -> ( DatePickerModel, Cmd DatePickerMsg )
datePickerInit id =
    ( { id = id
      , today = Nothing
      , indexDate = Nothing
      , currentMonthMap = Nothing
      , previousMonthMap = Nothing
      , selectedDate = Nothing
      , previousSelectedDate = Nothing
      , selectionMode = Calendar
      , monthChange = Next
      , yearList = []
      , colors =
            Dict.empty
                |> Dict.insert "primary" "#5a9789"
      }
    , Task.perform GetToday Date.now
    )


{-| datePickerUpdate consumes the message you've mapped and a `DatePickerModel` to output `( DatePickerModel, Cmd DatePickerMsg)`.
You will need to alter your update function to handle `DatePickerMsg`'s that flow through and allow it to update accordingly.

    import DatePicker exposing (datePickerUpdate, DatePickerMsg(SelectDate))

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            NoOp ->
                ( model, Cmd.none )

            OnDatePickerMsg datePickerMsg ->
                -- first use datePickerUpdate to get updated datePickerData
                datePickerUpdate datePickerMsg model.datePickerData
                    |> (\( data, cmd ) ->
                        ( { model | datePickerData = data }
                        , Cmd.map OnDatePickerMsg cmd
                        )
                    )
                    -- and now we can respond to any internal messages we want
                    |> (\( model, cmd ) ->
                        case datePickerMsg of
                            SubmitClicked currentSelectedDate ->
                                ( { model | selectedDate = Just currentSelectedDate }
                                , cmd
                                )

                            _ ->
                                ( model, cmd )
                    )
-}
datePickerUpdate : DatePickerMsg -> DatePickerModel -> ( DatePickerModel, Cmd DatePickerMsg )
datePickerUpdate msg model =
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


{-| The second argument passed to datePickerView. These are configuration properties
and other information that lives outside the DatePickerModel.
-}
type alias DatePickerProps =
    { canSelect : Date -> Bool
    }


getDayMonthText date =
    let
        ( monthFull, monthInt ) =
            getMonthInfo <| Date.month date

        ( dayShort, dayInt ) =
            getDayInfo <| Date.dayOfWeek date
    in
        dayShort ++ ", " ++ (String.slice 0 3 monthFull) ++ " " ++ (toString <| Date.day date)


headerSection : InitializedModel -> DatePickerProps -> Html DatePickerMsg
headerSection model props =
    let
        ( yearText, dayMonthText ) =
            case model.selectedDate of
                Just selected ->
                    ( toString <| Date.year selected
                    , getDayMonthText selected
                    )

                Nothing ->
                    ( toString <| Date.year model.indexDate, getDayMonthText model.indexDate )
    in
        div
            [ class "edp-header-section"
            , style [ ( "background-color", getColor model "primary" ) ]
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


monthChangeSection : InitializedModel -> DatePickerProps -> Html DatePickerMsg
monthChangeSection model props =
    div
        [ class "edp-month-change-section edp-body-section"
        ]
        [ div
            [ onClick (PreviousMonth (add Month -1 model.indexDate))
            ]
            [ i [ class "material-icons arrow-icon" ] [ text "keyboard_arrow_left" ]
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
            [ onClick (NextMonth (add Month 1 model.indexDate))
            ]
            [ i [ class "material-icons arrow-icon" ] [ text "keyboard_arrow_right" ]
            ]
        ]


weekSection : InitializedModel -> DatePickerProps -> Html DatePickerMsg
weekSection model props =
    div [ class "edp-body-section" ]
        (List.map
            (\symbol ->
                div [ class "edp-column edp-day-symbol" ] [ text symbol ]
            )
            [ "S", "M", "T", "W", "T", "F", "S" ]
        )


daySectionMonth : InitializedModel -> DatePickerProps -> Html DatePickerMsg
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
                        props.canSelect date
                in
                    div
                        [ classList
                            [ ( "edp-column edp-day-number", True )
                            , ( "edp-empty-column", dayNum == 0 )
                            , ( "edp-disabled-column", canSelect == False )
                            ]
                        , style
                            (if isSelected then
                                [ ( "background-color", getColor model "primary" ), ( "color", "white" ) ]
                             else if isToday then
                                [ ( "color", getColor model "primary" ), ( "font-weight", "bold" ) ]
                             else
                                []
                            )
                        , onClick (DateSelected (setDayOfMonth model.indexDate dayNum) (Maybe.withDefault model.indexDate model.selectedDate))
                        ]
                        [ text
                            (toString dayNum)
                        ]
            )
            model.currentMonthMap
        )


daySection : InitializedModel -> DatePickerProps -> Html DatePickerMsg
daySection model props =
    Keyed.node "div"
        [ class "edp-month-wrapper"
        ]
        (case model.previousMonthMap of
            Just previousMonthMap ->
                let
                    monthString =
                        toString (Tuple.first <| getMonthInfo (Date.month model.indexDate))
                in
                    [ ( monthString ++ "-previous"
                      , (div
                            [ classList
                                [ ( "edp-month-slider", True )
                                , ( "edp-out-next", model.monthChange == Next )
                                , ( "edp-out-previous", model.monthChange /= Next )
                                ]
                            ]
                            [ daySectionMonth { model | currentMonthMap = previousMonthMap } props
                            ]
                        )
                      )
                    , ( monthString
                      , div
                            [ class
                                ("edp-month-slider "
                                    ++ if model.monthChange == Next then
                                        "edp-in-next"
                                       else
                                        "edp-in-previous"
                                )
                            ]
                            [ daySectionMonth model props
                            ]
                      )
                    ]

            Nothing ->
                [ ( "key.only"
                  , div [ class "edp-month-slider" ]
                        [ daySectionMonth model props
                        ]
                  )
                ]
        )


bottomSection : InitializedModel -> DatePickerProps -> Html DatePickerMsg
bottomSection model props =
    let
        disableOk =
            model.selectedDate == Nothing

        okButtonColor =
            if disableOk == False then
                (getColor model "primary")
            else
                "rgba(0, 0, 0, 0.24)"
    in
        div [ class "edp-body-section edp-bottom-section" ]
            [ button
                [ onClick CancelClicked
                , class "edp-button"
                , style [ ( "color", getColor model "primary" ) ]
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
                , style [ ( "color", okButtonColor ) ]
                ]
                [ text "OK" ]
            ]


yearPickerSection : InitializedModel -> DatePickerProps -> Html DatePickerMsg
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
                                    ]
                                , style
                                    [ ( "height", "40px" )
                                    , ( "line-height", "40px" )
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
The main view for the datepicker
-}
datePickerView : DatePickerModel -> DatePickerProps -> Html DatePickerMsg
datePickerView model props =
    let
        result =
            Maybe.map3
                (\today indexDate currentMonthMap ->
                    let
                        initializedModel =
                            { id = model.id
                            , today = today
                            , indexDate = indexDate
                            , selectedDate = model.selectedDate
                            , previousSelectedDate = model.previousSelectedDate
                            , colors = model.colors
                            , currentMonthMap = currentMonthMap
                            , previousMonthMap = model.previousMonthMap
                            , monthChange = model.monthChange
                            , yearList = model.yearList
                            , selectionMode = model.selectionMode
                            }
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
                                        , daySection initializedModel props
                                        , bottomSection initializedModel props
                                        ]

                                    YearPicker ->
                                        [ yearPickerSection initializedModel props
                                        , bottomSection initializedModel props
                                        ]
                                )
                            ]
                )
                model.today
                model.indexDate
                model.currentMonthMap
    in
        case result of
            Just initialized ->
                initialized

            Nothing ->
                div [ class "edp-container" ] []

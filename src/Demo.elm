module Demo exposing (..)

import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Date exposing (..)
import DatePicker exposing (DatePickerModel, DatePickerMsg, DatePickerMsg(..), datePickerInit, datePickerUpdate, datePickerView)


type alias Model =
    { selectedDate : Maybe Date
    , datePickerData : DatePickerModel
    }


type Msg
    = NoOp
    | OnDatePickerMsg DatePickerMsg


init : ( Model, Cmd Msg )
init =
    let
        ( datePickerData, datePickerCmd ) =
            datePickerInit "my-datepicker"
    in
        ( { datePickerData = datePickerData
          , selectedDate = Nothing
          }
        , Cmd.map OnDatePickerMsg datePickerCmd
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnDatePickerMsg datePickerMsg ->
            datePickerUpdate datePickerMsg model.datePickerData
                -- set the data returned from datePickerUpdate. Don't discard the command!
                |>
                    (\( data, cmd ) ->
                        ( { model | datePickerData = data }
                        , Cmd.map OnDatePickerMsg cmd
                        )
                    )
                -- and now we can respond to any internal messages we want
                |>
                    (\( model, cmd ) ->
                        case datePickerMsg of
                            SubmitClicked currentSelectedDate ->
                                ( { model | selectedDate = Just currentSelectedDate }
                                , cmd
                                )

                            _ ->
                                ( model, cmd )
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        message =
            (List.foldr
                (\character word -> (word ++ character))
                ""
                [ "f", "o", "o" ]
            )
    in
        div
            [ style
                [ ( "display", "flex" )
                , ( "justify-content", "center" )
                ]
            ]
            [ h3 [] [ text message ]
            , div
                [ style
                    [ ( "margin-top", "40px" )
                    , ( "box-shadow", "0 1px 3px rgba(0, 0, 0, 0.24)" )
                    ]
                ]
                [ datePickerView
                    model.datePickerData
                    { canSelect = (\date -> True)
                    }
                    |> Html.map OnDatePickerMsg
                ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

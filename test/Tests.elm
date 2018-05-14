module Tests exposing (..)

import Html
import Test exposing (..)
import Expect
import Test.Runner.Html exposing (run)
import Date
import Date.Extra.Create exposing (..)
import DatePicker.Util exposing (..)


suite : Test
suite =
    describe "datepicker util"
        [ test "getLastDayOfMonth" <|
            \_ ->
                let
                    date =
                        dateFromFields 2018 Date.May 0 0 0 0 0
                in
                    Expect.equal
                        (getLastDayOfMonth date 0)
                        31
        ]


main =
    run suite

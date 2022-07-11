module UnitTests exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Unit


suite : Test
suite =
    describe "The Unit module"
        [ test "[toSymbol] Converts celcius to °C" <|
            \_ ->
                Unit.toSymbol Unit.C |> Expect.equal "°C"
        , test "[toSymbol] Converts farenheit to °F" <|
            \_ ->
                Unit.toSymbol Unit.F |> Expect.equal "°F"
        , test "[toSymbol] Converts kelvin to °K" <|
            \_ ->
                Unit.toSymbol Unit.K |> Expect.equal "°K"
        ]

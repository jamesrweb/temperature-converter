module TemperatureTests exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..), equal, within)
import Fuzz exposing (float, string)
import Temperature
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "The Temperature module"
        [ test "[fromString] Initialises a bad input string to 0°" <|
            \_ ->
                Temperature.fromString "abc"
                    |> Temperature.toFloat
                    |> Expect.equal 0
        , test "[fromString] Initialises an integer input string to its value" <|
            \_ ->
                Temperature.fromString "2"
                    |> Temperature.toFloat
                    |> Expect.equal 2
        , test "[fromString] Initialises a floating point input string to its value" <|
            \_ ->
                Temperature.fromString "123.456"
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace 123.456
        , fuzz string "[fromString] Initialises a temperature from given input strings" <|
            \input ->
                Temperature.fromString input
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace (Maybe.withDefault 0 (String.toFloat input))
        , test "[fromFloat] Initialises a temperature from a floating input value" <|
            \_ ->
                Temperature.fromFloat 0.02
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace 0.02
        , test "[fromFloat] Initialises a temperature from an integer input value" <|
            \_ ->
                Temperature.fromFloat 7
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace 7
        , fuzz float "[fromFloat] Initialises a temperature from any value capable of being represented as a float" <|
            \value ->
                Temperature.fromFloat value
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace value
        , test "[toString] Transforms a given temperature to a string representing its value" <|
            \_ ->
                Temperature.fromFloat 1.2345
                    |> Temperature.toString
                    |> Expect.equal "1.23"
        , test "[toFloat] Transforms a given temperature to a float representing its value" <|
            \_ ->
                Temperature.fromString "2345.123"
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace 2345.123
        , test "[celciusToFarenheit] Converts 0°C to 32°F" <|
            \_ ->
                Temperature.fromFloat 0
                    |> Temperature.celciusToFarenheit
                    |> Temperature.toFloat
                    |> equal 32
        , test "[celciusToKelvin] Converts 0°C to 273.15°K" <|
            \_ ->
                Temperature.fromFloat 0
                    |> Temperature.celciusToKelvin
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace 273.15
        , test "[farneheitToCelcius] Converts 0°F to -17.7778°C" <|
            \_ ->
                Temperature.fromFloat 0
                    |> Temperature.farneheitToCelcius
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace -17.7778
        , test "[farneheitToKelvin] Converts 0°F to 255.372°K" <|
            \_ ->
                Temperature.fromFloat 0
                    |> Temperature.farneheitToKelvin
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace 255.372
        , test "[kelvinToCelcius] Converts 0°K to -273.15°C" <|
            \_ ->
                Temperature.fromFloat 0
                    |> Temperature.kelvinToCelcius
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace -273.15
        , test "[kelvinToFarenheit] Converts 0°K to -459.67°F" <|
            \_ ->
                Temperature.fromFloat 0
                    |> Temperature.kelvinToFarenheit
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace -459.67
        , fuzz float "[celciusToFarenheit] Converts a given °C value to °F correctly" <|
            \celcius ->
                Temperature.fromFloat celcius
                    |> Temperature.celciusToFarenheit
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace (celcius * (9 / 5) + 32)
        , fuzz float "[celciusToKelvin] Converts a given °C value to °K correctly" <|
            \celcius ->
                Temperature.fromFloat celcius
                    |> Temperature.celciusToKelvin
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace (celcius + 273.15)
        , fuzz float "[farneheitToCelcius] Converts a given °F value to °C correctly" <|
            \farenheit ->
                Temperature.fromFloat farenheit
                    |> Temperature.farneheitToCelcius
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace ((farenheit - 32) * (5 / 9))
        , fuzz float "[farneheitToKelvin] Converts a given °F value to °K correctly" <|
            \farenheit ->
                Temperature.fromFloat farenheit
                    |> Temperature.farneheitToKelvin
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace ((farenheit - 32) * (5 / 9) + 273.15)
        , fuzz float "[kelvinToCelcius] Converts a given °K value to °C correctly" <|
            \kelvin ->
                Temperature.fromFloat kelvin
                    |> Temperature.kelvinToCelcius
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace (kelvin - 273.15)
        , fuzz float "[kelvinToFarenheit] Converts a given °K value to °F correctly" <|
            \kelvin ->
                Temperature.fromFloat kelvin
                    |> Temperature.kelvinToFarenheit
                    |> Temperature.toFloat
                    |> expectWithinOneDecimalPlace ((kelvin - 273.15) * (9 / 5) + 32)
        ]


expectWithinOneDecimalPlace : Float -> Float -> Expectation
expectWithinOneDecimalPlace expected actual =
    within (AbsoluteOrRelative 0.01 0.01) expected actual

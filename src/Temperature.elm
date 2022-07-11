module Temperature exposing (Celcius(..), Farenheit(..), Kelvin(..), Temperature(..), celciusToFarenheit, celciusToKelvin, farneheitToCelcius, farneheitToKelvin, fromFloat, fromString, kelvinToCelcius, kelvinToFarenheit, toFloat, toString)

import Round exposing (roundNum)


type Celcius
    = Celcius


type Farenheit
    = Farenheit


type Kelvin
    = Kelvin


type Temperature a
    = Temperature Float


fromString : String -> Temperature a
fromString value =
    String.toFloat value |> Maybe.withDefault 0 |> fromFloat


fromFloat : Float -> Temperature a
fromFloat input =
    input |> roundNum 2 |> Temperature


toString : Temperature a -> String
toString temperature =
    toFloat temperature |> String.fromFloat


toFloat : Temperature a -> Float
toFloat (Temperature value) =
    value


celciusToFarenheit : Temperature Celcius -> Temperature Farenheit
celciusToFarenheit celcius =
    toFloat celcius * (9 / 5) + 32 |> fromFloat


celciusToKelvin : Temperature Celcius -> Temperature Kelvin
celciusToKelvin celcius =
    toFloat celcius + 273.15 |> fromFloat


farneheitToCelcius : Temperature Farenheit -> Temperature Celcius
farneheitToCelcius farenheit =
    (toFloat farenheit - 32) * (5 / 9) |> fromFloat


farneheitToKelvin : Temperature Farenheit -> Temperature Kelvin
farneheitToKelvin farenheit =
    (toFloat farenheit - 32) * (5 / 9) + 273.15 |> fromFloat


kelvinToCelcius : Temperature Kelvin -> Temperature Celcius
kelvinToCelcius kelvin =
    toFloat kelvin - 273.15 |> fromFloat


kelvinToFarenheit : Temperature Kelvin -> Temperature Farenheit
kelvinToFarenheit kelvin =
    (toFloat kelvin - 273.15) * (9 / 5) + 32 |> fromFloat

module Main exposing (Flags, Message, Model, main)

import Browser exposing (sandbox)
import Html exposing (Html, fieldset, form, input, label, legend, main_, strong, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Temperature exposing (Celcius, Farenheit, Kelvin, Temperature(..))
import Unit exposing (Unit(..))


type alias Flags =
    ()


main : Program Flags Model Message
main =
    sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { celcius : Temperature Celcius
    , farenheit : Temperature Farenheit
    , kelvin : Temperature Kelvin
    , baseUnit : Unit
    }


init : Model
init =
    { farenheit = Temperature 32
    , celcius = Temperature 0
    , kelvin = Temperature 273.15
    , baseUnit = C
    }


type Message
    = TemperatureFormSubmitted
    | CelciusChanged String
    | FarenheitChanged String
    | KelvinChanged String


update : Message -> Model -> Model
update message model =
    case message of
        TemperatureFormSubmitted ->
            model

        CelciusChanged value ->
            let
                celcius : Temperature Celcius
                celcius =
                    Temperature.fromString value

                farenheit : Temperature Farenheit
                farenheit =
                    Temperature.celciusToFarenheit celcius

                kelvin : Temperature Kelvin
                kelvin =
                    Temperature.celciusToKelvin celcius
            in
            { model | celcius = celcius, farenheit = farenheit, kelvin = kelvin, baseUnit = C }

        FarenheitChanged value ->
            let
                farenheit : Temperature Farenheit
                farenheit =
                    Temperature.fromString value

                celcius : Temperature Celcius
                celcius =
                    Temperature.farneheitToCelcius farenheit

                kelvin : Temperature Kelvin
                kelvin =
                    Temperature.farneheitToKelvin farenheit
            in
            { model | farenheit = farenheit, celcius = celcius, kelvin = kelvin, baseUnit = F }

        KelvinChanged value ->
            let
                kelvin : Temperature Kelvin
                kelvin =
                    Temperature.fromString value

                celcius : Temperature Celcius
                celcius =
                    Temperature.kelvinToCelcius kelvin

                farenheit : Temperature Farenheit
                farenheit =
                    Temperature.kelvinToFarenheit kelvin
            in
            { model | kelvin = kelvin, celcius = celcius, farenheit = farenheit, baseUnit = K }


view : Model -> Html Message
view model =
    main_ []
        [ conversionForm model
        , conversionOutput model
        ]


conversionForm : Model -> Html Message
conversionForm model =
    form [ onSubmit TemperatureFormSubmitted ]
        [ fieldset []
            [ legend [] [ text "Temperature converter" ]
            , conversionInput CelciusChanged model.celcius "Celcius"
            , conversionInput FarenheitChanged model.farenheit "Farenheit"
            , conversionInput KelvinChanged model.kelvin "Kelvin"
            ]
        ]


conversionInput : (String -> Message) -> Temperature a -> String -> Html Message
conversionInput inputMessage temperature labelText =
    label [ style "display" "block" ]
        [ text labelText
        , input
            [ type_ "text"
            , onInput inputMessage
            , value (Temperature.toString temperature)
            ]
            []
        ]


conversionOutput : Model -> Html Message
conversionOutput model =
    strong [] [ conversionOutputMessage model |> text ]


conversionOutputMessage : Model -> String
conversionOutputMessage model =
    let
        c : ( Temperature Celcius, Unit )
        c =
            ( model.celcius, C )

        f : ( Temperature Farenheit, Unit )
        f =
            ( model.farenheit, F )

        k : ( Temperature Kelvin, Unit )
        k =
            ( model.kelvin, K )
    in
    case model.baseUnit of
        C ->
            conversionOutputMessageFormatter c f k

        F ->
            conversionOutputMessageFormatter f c k

        K ->
            conversionOutputMessageFormatter k c f


conversionOutputMessageFormatter : ( Temperature a, Unit ) -> ( Temperature b, Unit ) -> ( Temperature c, Unit ) -> String
conversionOutputMessageFormatter temperatureA temperatureB temperatureC =
    let
        x : String
        x =
            Temperature.toString (Tuple.first temperatureA) ++ Unit.toSymbol (Tuple.second temperatureA)

        y : String
        y =
            Temperature.toString (Tuple.first temperatureB) ++ Unit.toSymbol (Tuple.second temperatureB)

        z : String
        z =
            Temperature.toString (Tuple.first temperatureC) ++ Unit.toSymbol (Tuple.second temperatureC)
    in
    x ++ " is " ++ y ++ " and " ++ z ++ "."

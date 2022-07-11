module Main exposing (Flags, Message, Model, main)

import Browser exposing (sandbox)
import Html exposing (Html, fieldset, form, input, label, legend, main_, strong, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Temperature exposing (Kelvin, Temperature(..))
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
    { celcius : Temperature Temperature.Celcius
    , farenheit : Temperature Temperature.Farenheit
    , kelvin : Temperature Temperature.Kelvin
    , lastUpdated : Unit
    }


init : Model
init =
    { farenheit = Temperature 32
    , celcius = Temperature 0
    , kelvin = Temperature 273.15
    , lastUpdated = C
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
                celcius : Temperature Temperature.Celcius
                celcius =
                    Temperature.fromString value

                farenheit : Temperature Temperature.Farenheit
                farenheit =
                    Temperature.celciusToFarenheit celcius

                kelvin : Temperature Kelvin
                kelvin =
                    Temperature.celciusToKelvin celcius
            in
            { model | celcius = celcius, farenheit = farenheit, kelvin = kelvin, lastUpdated = C }

        FarenheitChanged value ->
            let
                farenheit : Temperature Temperature.Farenheit
                farenheit =
                    Temperature.fromString value

                celcius : Temperature Temperature.Celcius
                celcius =
                    Temperature.farneheitToCelcius farenheit

                kelvin : Temperature Kelvin
                kelvin =
                    Temperature.farneheitToKelvin farenheit
            in
            { model | farenheit = farenheit, celcius = celcius, kelvin = kelvin, lastUpdated = F }

        KelvinChanged value ->
            let
                kelvin : Temperature Temperature.Kelvin
                kelvin =
                    Temperature.fromString value

                celcius : Temperature Temperature.Celcius
                celcius =
                    Temperature.kelvinToCelcius kelvin

                farenheit : Temperature Temperature.Farenheit
                farenheit =
                    Temperature.kelvinToFarenheit kelvin
            in
            { model | kelvin = kelvin, celcius = celcius, farenheit = farenheit, lastUpdated = K }


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
            , label [ style "display" "block" ]
                [ text "Celcius:"
                , input [ type_ "text", onInput CelciusChanged, value (Temperature.toString model.celcius) ] []
                ]
            , label [ style "display" "block" ]
                [ text "Farenheit:"
                , input [ type_ "text", onInput FarenheitChanged, value (Temperature.toString model.farenheit) ] []
                ]
            , label [ style "display" "block" ]
                [ text "Kelvin:"
                , input [ type_ "text", onInput KelvinChanged, value (Temperature.toString model.kelvin) ] []
                ]
            ]
        ]


conversionOutput : Model -> Html Message
conversionOutput model =
    strong [] [ conversionOutputMessage model |> text ]


conversionOutputMessage : Model -> String
conversionOutputMessage model =
    let
        c : ( Temperature Temperature.Celcius, Unit )
        c =
            ( model.celcius, C )

        f : ( Temperature Temperature.Farenheit, Unit )
        f =
            ( model.farenheit, F )

        k : ( Temperature Temperature.Kelvin, Unit )
        k =
            ( model.kelvin, K )
    in
    case model.lastUpdated of
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

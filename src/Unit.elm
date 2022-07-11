module Unit exposing (Unit(..), toSymbol)


type Unit
    = C
    | F
    | K


toSymbol : Unit -> String
toSymbol temperature =
    case temperature of
        C ->
            "°C"

        F ->
            "°F"

        K ->
            "°K"

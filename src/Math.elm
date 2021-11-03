module Math exposing (..)


roundTo : Int -> Float -> Float
roundTo dp val =
    let
        factor =
            toFloat (10 ^ dp)
    in
    toFloat (round (val * factor)) / factor


standardDeviation : List Int -> Float
standardDeviation vals =
    let
        mean =
            toFloat (List.sum vals) / toFloat (List.length vals)

        standardSum =
            vals |> List.map toFloat |> List.map (\x -> (x - mean) * (x - mean)) |> List.sum

        sizeMinus1 =
            List.length vals - 1 |> toFloat
    in
    sqrt (standardSum / sizeMinus1)

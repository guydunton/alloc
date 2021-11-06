module Math exposing (..)


roundTo : Int -> Float -> Float
roundTo dp val =
    let
        factor =
            toFloat (10 ^ dp)
    in
    toFloat (round (val * factor)) / factor


meanF : List Float -> Float
meanF vals =
    List.sum vals / toFloat (List.length vals)


standardDeviation : List Float -> Float
standardDeviation vals =
    let
        mean =
            meanF vals

        standardSum =
            vals |> List.map (\x -> (x - mean) * (x - mean)) |> List.sum

        sizeMinus1 =
            List.length vals - 1 |> toFloat
    in
    sqrt (standardSum / sizeMinus1)

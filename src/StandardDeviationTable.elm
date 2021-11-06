module StandardDeviationTable exposing (..)

import Animal exposing (Animal)
import Html exposing (Html, table, td, text, tr)
import Math exposing (meanF, roundTo, standardDeviation)
import Unique exposing (unique)


standardDeviationTable : List Animal -> Html a
standardDeviationTable animals =
    let
        headings =
            [ "Group", "Mean", "Standard deviations" ]

        groups =
            animals |> List.filterMap (\a -> a.group) |> unique

        headingRow =
            tr []
                (headings
                    |> List.map (\h -> td [] [ text h ])
                )

        groupBodyweights group =
            animals
                |> List.filter (\a -> a.group == Just group)
                |> List.map (\a -> a.bodyweight)

        groupRows =
            groups
                |> List.map
                    (\g ->
                        tr []
                            [ td [] [ text (String.fromInt g) ]
                            , td [] [ text <| String.fromFloat <| roundTo 3 <| meanF <| groupBodyweights g ]
                            , td [] [ text <| String.fromFloat <| roundTo 3 <| standardDeviation <| groupBodyweights g ]
                            ]
                    )
    in
    table [] (headingRow :: groupRows)

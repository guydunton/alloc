module StandardDeviationTable exposing (..)

import Animal exposing (Animal)
import Html exposing (Html, table, td, text, tr)
import Math exposing (meanF, roundTo, standardDeviation)


unique : List a -> List a
unique l =
    let
        incUnique : a -> List a -> List a
        incUnique elem lst =
            if List.member elem lst then
                lst

            else
                elem :: lst
    in
    List.foldr incUnique [] l


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

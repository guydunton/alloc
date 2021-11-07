module DamConflictTable exposing (..)

import Animal exposing (Animal)
import Html exposing (Html, table, td, text, th, tr)
import Unique exposing (unique)


type alias Dam =
    { damId : String
    , children : List Animal
    }


type alias GroupedChildren =
    { damId : String
    , childrenIds : List String
    , group : Int
    }


getDams : List Animal -> List Dam
getDams animals =
    let
        allDams =
            animals
                |> List.map (\a -> a.damId)
                |> unique
    in
    allDams |> List.map (\d -> { damId = d, children = animalsPerDam animals d })


animalsPerDam : List Animal -> String -> List Animal
animalsPerDam animals damId =
    animals |> List.filter (\a -> a.damId == damId)


areChildrenInSameGroup : Dam -> Bool
areChildrenInSameGroup dam =
    case dam.children of
        head :: tail ->
            List.all (\a -> a.group == head.group) tail

        _ ->
            False


convertDamToGroupedChildren : Dam -> Maybe GroupedChildren
convertDamToGroupedChildren dam =
    List.head dam.children
        |> Maybe.andThen (\a -> a.group)
        |> Maybe.map
            (\g ->
                { damId = dam.damId
                , childrenIds = List.map (\a -> a.supplierId) dam.children
                , group = g
                }
            )


damConflictTable : List Animal -> Html a
damConflictTable animals =
    let
        dams : List Dam
        dams =
            getDams animals

        groupedChildren : List GroupedChildren
        groupedChildren =
            dams
                |> List.filter areChildrenInSameGroup
                |> List.filterMap convertDamToGroupedChildren

        headingsRow =
            [ "Dam ID", "Siblings", "Group" ]
                |> List.map (\h -> th [] [ text h ])
                |> tr []

        siblingRows =
            groupedChildren |> List.map groupedChildrenAsRow
    in
    table [] (headingsRow :: siblingRows)


groupedChildrenAsRow : GroupedChildren -> Html a
groupedChildrenAsRow children =
    tr []
        [ td [] [ text children.damId ]
        , td [] [ text <| String.join ", " <| children.childrenIds ]
        , td [] [ text <| String.fromInt <| children.group ]
        ]

module BatchData exposing (..)

import Html exposing (Html, button, div, h2, input, table, td, text, th, tr)
import Html.Events exposing (onClick, onInput)


type alias Batch =
    { name : String
    }


type alias BatchData =
    { showBatches : Bool
    , batches : List Batch
    , newBatchText : String
    }


type BatchDataMsg
    = ToggleBatchData
    | DeleteBatch String
    | AddBatch
    | UpdateNewBatchText String


defaultBatchData : BatchData
defaultBatchData =
    { showBatches = False
    , batches =
        [ { name = "Male"
          }
        , { name = "Female"
          }
        ]
    , newBatchText = ""
    }


batchDataFromNames : List String -> BatchData
batchDataFromNames names =
    { showBatches = False
    , batches = List.map (\n -> { name = n }) names
    , newBatchText = ""
    }


updateBatchNames : List String -> BatchData -> BatchData
updateBatchNames names batchData =
    { batchData | batches = List.map (\n -> { name = n }) names }


getBatchNames : BatchData -> List String
getBatchNames batchData =
    batchData.batches |> List.map (\b -> b.name)


firstBatchName : BatchData -> Maybe String
firstBatchName batchData =
    batchData.batches
        |> List.map (\b -> b.name)
        |> List.head


updateBatchData : BatchDataMsg -> BatchData -> BatchData
updateBatchData msg model =
    case msg of
        ToggleBatchData ->
            { model | showBatches = not model.showBatches }

        DeleteBatch batchName ->
            { model | batches = List.filter (\b -> b.name /= batchName) model.batches }

        AddBatch ->
            { model | batches = model.batches ++ [ { name = model.newBatchText } ], newBatchText = "" }

        UpdateNewBatchText text ->
            { model | newBatchText = text }


viewBatchData : BatchData -> Html BatchDataMsg
viewBatchData model =
    div []
        ((if model.showBatches then
            [ batchesTable model ]

          else
            []
         )
            ++ [ button [ onClick ToggleBatchData ]
                    [ text
                        (if model.showBatches then
                            "Hide"

                         else
                            "Show Batches"
                        )
                    ]
               ]
        )


batchesTable : BatchData -> Html BatchDataMsg
batchesTable model =
    let
        batchRows =
            model.batches
                |> List.map
                    (\b ->
                        tr []
                            [ td [] [ text b.name ]
                            , td [] [ button [ onClick (DeleteBatch b.name) ] [ text "-" ] ]
                            ]
                    )
    in
    div []
        [ h2 [] [ text "Batch Data" ]
        , table []
            (tr [] [ th [] [ text "Batch" ], th [] [] ]
                :: batchRows
                ++ [ tr []
                        [ td [] [ input [ onInput UpdateNewBatchText ] [ text model.newBatchText ] ]
                        , td [] [ button [ onClick AddBatch ] [ text "+" ] ]
                        ]
                   ]
            )
        ]

module Main exposing (..)

import Animal exposing (Animal, starterAnimals)
import BatchData exposing (BatchData, BatchDataMsg, defaultBatchData, firstBatchName, updateBatchData, updateBatchNames, viewBatchData)
import Browser
import DamConflictTable exposing (damConflictTable)
import Html exposing (Html, button, div, h1, h2, input, option, select, table, td, text, th, tr)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import IO exposing (IOMsg, IOOutput(..), fileUpdate, ioDownloadFile, ioRequestFile)
import StandardDeviationTable exposing (standardDeviationTable)
import Unique exposing (unique)


type Fields
    = BodyWeight
    | SupplierId
    | DamId
    | Batch


type alias Model =
    { animals : List Animal
    , currentBodyWeight : String
    , currentSupplierId : String
    , currentDamId : String
    , currentBatch : String
    , batchData : BatchData
    }


type Msg
    = UpdateAnimalField Fields String
    | AddAnimal
    | EditGroup Int String
    | RemoveAnimal Int
    | IOAction IOMsg
    | BatchUpdate BatchDataMsg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { animals = starterAnimals
      , currentBodyWeight = ""
      , currentSupplierId = ""
      , currentDamId = ""
      , currentBatch = ""
      , batchData = defaultBatchData
      }
    , Cmd.none
    )


convertModelToAnimal : Model -> Maybe Animal
convertModelToAnimal model =
    let
        bodyWeight =
            model.currentBodyWeight |> String.toFloat
    in
    case bodyWeight of
        Just weight ->
            Just
                { bodyweight = weight
                , supplierId = model.currentSupplierId
                , damId = model.currentDamId
                , batch = model.currentBatch
                , group = Nothing
                }

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAnimalField field text ->
            let
                newModel =
                    case field of
                        BodyWeight ->
                            { model | currentBodyWeight = text }

                        SupplierId ->
                            { model | currentSupplierId = text }

                        DamId ->
                            { model | currentDamId = text }

                        Batch ->
                            { model | currentBatch = text }
            in
            ( newModel, Cmd.none )

        AddAnimal ->
            case convertModelToAnimal model of
                Just animal ->
                    ( { model
                        | animals = animal :: model.animals
                        , currentBodyWeight = ""
                        , currentDamId = ""
                        , currentSupplierId = ""
                        , currentBatch = Maybe.withDefault "" (firstBatchName model.batchData)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EditGroup index groupText ->
            let
                addGroupForIndexed i animal =
                    if index == i then
                        { animal | group = String.toInt groupText }

                    else
                        animal

                newAnimals =
                    model.animals
                        |> List.indexedMap addGroupForIndexed
            in
            ( { model | animals = newAnimals }, Cmd.none )

        RemoveAnimal index ->
            ( { model | animals = removeAt index model.animals }, Cmd.none )

        IOAction ioMsg ->
            let
                ioResult =
                    fileUpdate ioMsg model.animals
            in
            case ioResult of
                ( LoadedAnimals newAnimals, ioCmd ) ->
                    ( { model | animals = newAnimals, batchData = createBatchesFromAnimals newAnimals model.batchData }, ioCmd |> Cmd.map IOAction )

                ( Noop, ioCmd ) ->
                    ( model, ioCmd |> Cmd.map IOAction )

        BatchUpdate batchMsg ->
            ( { model | batchData = updateBatchData batchMsg model.batchData }, Cmd.none )


createBatchesFromAnimals : List Animal -> BatchData -> BatchData
createBatchesFromAnimals animals batchData =
    let
        batchNames =
            animals |> List.map (\a -> a.batch) |> unique
    in
    updateBatchNames batchNames batchData


removeAt : Int -> List a -> List a
removeAt index vals =
    let
        front =
            List.take index vals

        end =
            List.drop (index + 1) vals
    in
    List.append front end


view : Model -> Browser.Document Msg
view model =
    { title = "Allocation"
    , body =
        [ h1 []
            [ text "Allocation" ]
        , div []
            [ button [ onClick (IOAction ioDownloadFile) ] [ text "Save to file" ]
            , button [ onClick (IOAction ioRequestFile) ] [ text "Load file" ]
            ]
        , viewBatchData model.batchData |> Html.map (\x -> BatchUpdate x)
        , div []
            [ h2 [] [ text "Animals" ]
            , viewAnimals model.animals
            ]
        , div []
            [ h2 [] [ text "Add animals" ]
            , input [ onInput (UpdateAnimalField BodyWeight), value model.currentBodyWeight ] []
            , input [ onInput (UpdateAnimalField SupplierId), value model.currentSupplierId ] []
            , input [ onInput (UpdateAnimalField DamId), value model.currentDamId ] []
            , select [ onInput (UpdateAnimalField Batch), value model.currentBatch ]
                (BatchData.getBatchNames
                    model.batchData
                    |> List.map (\n -> option [] [ text n ])
                )
            , button [ onClick AddAnimal ] [ text "Add" ]
            ]
        , div []
            [ h2 [] [ text "Standard deviations" ]
            , standardDeviationTable model.animals
            ]
        , div []
            [ h2 [] [ text "Conflicing groups" ]
            , damConflictTable model.animals
            ]
        ]
    }


viewAnimals : List Animal -> Html Msg
viewAnimals animals =
    animals
        |> List.indexedMap viewAnimal
        |> List.append
            [ tr []
                [ th [] [ text "Bodyweight" ]
                , th [] [ text "Supplier ID" ]
                , th [] [ text "Dam ID" ]
                , th [] [ text "Gender" ]
                , th [] [ text "Group" ]
                , th [] [ text "Set Group" ]
                ]
            ]
        |> table []


viewAnimal : Int -> Animal -> Html Msg
viewAnimal index animal =
    let
        group =
            case animal.group of
                Just grp ->
                    String.fromInt grp

                _ ->
                    ""
    in
    tr []
        [ td [] [ text (String.fromFloat animal.bodyweight) ]
        , td [] [ text animal.supplierId ]
        , td [] [ text animal.damId ]
        , td [] [ text animal.batch ]
        , td [] [ text group ]
        , td [] [ input [ onInput (EditGroup index), value group ] [] ]
        , td [] [ button [ onClick (RemoveAnimal index) ] [ text "-" ] ]
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

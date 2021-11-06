module Main exposing (..)

import Animal exposing (Animal, Sex(..), sexToString, starterAnimals, textToSex)
import Browser exposing (sandbox)
import DamConflictTable exposing (damConflictTable)
import Html exposing (Html, button, div, h1, h2, input, option, select, table, td, text, th, tr)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import StandardDeviationTable exposing (standardDeviationTable)


type Fields
    = BodyWeight
    | SupplierId
    | DamId
    | Gender


type alias Model =
    { animals : List Animal
    , currentBodyWeight : String
    , currentSupplierId : String
    , currentDamId : String
    , currentGender : Maybe Sex
    }


type Msg
    = UpdateAnimalField Fields String
    | AddAnimal
    | EditGroup Int String
    | RemoveAnimal Int


init : Model
init =
    { animals = starterAnimals
    , currentBodyWeight = ""
    , currentSupplierId = ""
    , currentDamId = ""
    , currentGender = Just Male
    }


convertModelToAnimal : Model -> Maybe Animal
convertModelToAnimal model =
    let
        bodyWeight =
            model.currentBodyWeight |> String.toFloat
    in
    case ( bodyWeight, model.currentGender ) of
        ( Just weight, Just gender ) ->
            Just
                { bodyweight = weight
                , supplierId = model.currentSupplierId
                , damId = model.currentDamId
                , sex = gender
                , group = Nothing
                }

        _ ->
            Nothing


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateAnimalField field text ->
            case field of
                BodyWeight ->
                    { model | currentBodyWeight = text }

                SupplierId ->
                    { model | currentSupplierId = text }

                DamId ->
                    { model | currentDamId = text }

                Gender ->
                    { model | currentGender = textToSex text }

        AddAnimal ->
            case convertModelToAnimal model of
                Just animal ->
                    { model
                        | animals = animal :: model.animals
                        , currentBodyWeight = ""
                        , currentDamId = ""
                        , currentSupplierId = ""
                        , currentGender = Just Male
                    }

                _ ->
                    model

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
            { model | animals = newAnimals }

        RemoveAnimal index ->
            { model | animals = removeAt index model.animals }


removeAt : Int -> List a -> List a
removeAt index vals =
    let
        front =
            List.take index vals

        end =
            List.drop (index + 1) vals
    in
    List.append front end


view : Model -> Html Msg
view model =
    let
        currentGenderString =
            model.currentGender |> Maybe.map sexToString |> Maybe.withDefault ""
    in
    div []
        [ h1 []
            [ text "Allocation" ]
        , div []
            [ viewAnimals model.animals
            ]
        , div []
            [ h2 [] [ text "Add animals" ]
            , input [ onInput (UpdateAnimalField BodyWeight), value model.currentBodyWeight ] []
            , input [ onInput (UpdateAnimalField SupplierId), value model.currentSupplierId ] []
            , input [ onInput (UpdateAnimalField DamId), value model.currentDamId ] []
            , select [ onInput (UpdateAnimalField Gender), value currentGenderString ]
                [ option [] [ text "Male" ]
                , option [] [ text "Female" ]
                ]
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
        [ td [] [ text <| String.fromFloat animal.bodyweight ]
        , td [] [ text animal.supplierId ]
        , td [] [ text animal.damId ]
        , td []
            [ text
                (case animal.sex of
                    Male ->
                        "Male"

                    Female ->
                        "Female"
                )
            ]
        , td [] [ text group ]
        , td [] [ input [ onInput (EditGroup index), value group ] [] ]
        , td [] [ button [ onClick (RemoveAnimal index) ] [ text "Del" ] ]
        ]


main : Program () Model Msg
main =
    sandbox
        { init = init
        , update = update
        , view = view
        }

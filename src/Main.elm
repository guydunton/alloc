module Main exposing (..)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, h1, h2, input, option, select, table, td, text, th, tr)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)


type Sex
    = Male
    | Female


type Fields
    = BodyWeight
    | SupplierId
    | DamId
    | Gender


type alias Animal =
    { bodyweight : Float
    , supplierId : String
    , damId : String
    , sex : Sex
    , group : Maybe Int
    }


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


init : Model
init =
    { animals =
        [ { bodyweight = 1.0
          , supplierId = "m1"
          , damId = "d1"
          , sex = Male
          , group = Nothing
          }
        ]
    , currentBodyWeight = ""
    , currentSupplierId = ""
    , currentDamId = ""
    , currentGender = Just Male
    }


sexToString : Sex -> String
sexToString sex =
    case sex of
        Male ->
            "Male"

        Female ->
            "Female"


textToSex : String -> Maybe Sex
textToSex text =
    case text of
        "Male" ->
            Just Male

        "Female" ->
            Just Female

        _ ->
            Nothing


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
        , div [] []
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
        ]


main : Program () Model Msg
main =
    sandbox
        { init = init
        , update = update
        , view = view
        }

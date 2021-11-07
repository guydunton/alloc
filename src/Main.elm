module Main exposing (..)

import Animal exposing (Animal, starterAnimals)
import Browser exposing (Document)
import DamConflictTable exposing (damConflictTable)
import Gender exposing (Gender(..), genderToString, textToGender)
import Html exposing (Html, button, div, h1, h2, input, option, select, table, td, text, th, tr)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import IO exposing (IOMsg, IOOutput(..), fileUpdate, ioDownloadFile, ioRequestFile)
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
    , currentGender : Maybe Gender
    }


type Msg
    = UpdateAnimalField Fields String
    | AddAnimal
    | EditGroup Int String
    | RemoveAnimal Int
    | IOAction IOMsg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { animals = starterAnimals
      , currentBodyWeight = ""
      , currentSupplierId = ""
      , currentDamId = ""
      , currentGender = Just Male
      }
    , Cmd.none
    )


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
                , gender = gender
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

                        Gender ->
                            { model | currentGender = textToGender text }
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
                        , currentGender = Just Male
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
                    ( { model | animals = newAnimals }, ioCmd |> Cmd.map IOAction )

                ( Noop, ioCmd ) ->
                    ( model, ioCmd |> Cmd.map IOAction )


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
    let
        currentGenderString =
            model.currentGender |> Maybe.map genderToString |> Maybe.withDefault ""
    in
    { title = "Allocation"
    , body =
        [ h1 []
            [ text "Allocation" ]
        , div []
            [ button [ onClick (IOAction ioDownloadFile) ] [ text "Save to file" ]
            , button [ onClick (IOAction ioRequestFile) ] [ text "Load file" ]
            ]
        , div []
            [ h2 [] [ text "Animals" ]
            , viewAnimals model.animals
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
        , td [] [ text (genderToString animal.gender) ]
        , td [] [ text group ]
        , td [] [ input [ onInput (EditGroup index), value group ] [] ]
        , td [] [ button [ onClick (RemoveAnimal index) ] [ text "Del" ] ]
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

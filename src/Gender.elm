module Gender exposing (..)

import Csv.Decode as D exposing (Decoder)


type Gender
    = Male
    | Female


genderDecoder : Decoder Gender
genderDecoder =
    D.string
        |> D.map textToGender
        |> D.map (Maybe.withDefault Male)


genderToString : Gender -> String
genderToString gender =
    case gender of
        Male ->
            "Male"

        Female ->
            "Female"


textToGender : String -> Maybe Gender
textToGender text =
    case text of
        "Male" ->
            Just Male

        "Female" ->
            Just Female

        _ ->
            Nothing

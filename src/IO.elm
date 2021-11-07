module IO exposing
    ( IOMsg
    , IOOutput(..)
    , fileUpdate
    , ioDownloadFile
    , ioRequestFile
    )

import Animal exposing (Animal, animalEncode, decodeAnimal)
import Csv.Decode exposing (FieldNames(..), decodeCsv)
import Csv.Encode exposing (encode)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Task


ioDownloadFile : IOMsg
ioDownloadFile =
    Download


ioRequestFile : IOMsg
ioRequestFile =
    Requested


type IOMsg
    = Download
    | Requested
    | Selected File
    | Loaded String


type IOOutput
    = LoadedAnimals (List Animal)
    | Noop


fileUpdate : IOMsg -> List Animal -> ( IOOutput, Cmd IOMsg )
fileUpdate msg animals =
    case msg of
        Download ->
            ( Noop
            , Download.string "animals.csv"
                "text/csv"
                (encode
                    { encoder = animalEncode
                    , fieldSeparator = ','
                    }
                    animals
                )
            )

        Requested ->
            ( Noop, Select.file [ "text/csv" ] Selected )

        Selected file ->
            ( Noop
            , Task.perform Loaded (File.toString file)
            )

        Loaded content ->
            let
                decodedFile : Result Csv.Decode.Error (List Animal)
                decodedFile =
                    decodeCsv FieldNamesFromFirstRow decodeAnimal content
            in
            ( case decodedFile of
                Ok newAnimals ->
                    LoadedAnimals newAnimals

                _ ->
                    Noop
            , Cmd.none
            )

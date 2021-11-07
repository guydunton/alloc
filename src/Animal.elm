module Animal exposing (..)

import Csv.Decode exposing (Decoder, blank, field, float, int, into, pipeline, string)
import Csv.Encode exposing (Encoder, withFieldNames)
import Gender exposing (Gender(..), genderDecoder, genderToString)


type alias Animal =
    { bodyweight : Float
    , supplierId : String
    , damId : String
    , gender : Gender
    , group : Maybe Int
    }


animalEncode : Encoder Animal
animalEncode =
    withFieldNames
        (\animal ->
            [ ( "bodyweight", String.fromFloat animal.bodyweight )
            , ( "supplierid", animal.supplierId )
            , ( "damid", animal.damId )
            , ( "gender", genderToString animal.gender )
            , ( "group"
              , case animal.group of
                    Just grp ->
                        String.fromInt grp

                    _ ->
                        ""
              )
            ]
        )


decodeAnimal : Decoder Animal
decodeAnimal =
    into Animal
        |> pipeline (field "bodyweight" float)
        |> pipeline (field "supplierid" string)
        |> pipeline (field "damid" string)
        |> pipeline (field "gender" genderDecoder)
        |> pipeline (field "group" (blank int))


starterAnimals : List Animal
starterAnimals =
    [ { bodyweight = 1.1
      , supplierId = "m1"
      , damId = "d1"
      , gender = Male
      , group = Just 1
      }
    , { bodyweight = 2.1
      , supplierId = "m2"
      , damId = "d2"
      , gender = Male
      , group = Just 2
      }
    , { bodyweight = 3.1
      , supplierId = "m3"
      , damId = "d3"
      , gender = Male
      , group = Just 3
      }
    , { bodyweight = 4.1
      , supplierId = "m4"
      , damId = "d4"
      , gender = Male
      , group = Just 4
      }
    , { bodyweight = 5.1
      , supplierId = "f1"
      , damId = "d1"
      , gender = Female
      , group = Just 1
      }
    , { bodyweight = 6.1
      , supplierId = "f2"
      , damId = "d2"
      , gender = Female
      , group = Just 2
      }
    , { bodyweight = 7.1
      , supplierId = "f3"
      , damId = "d3"
      , gender = Female
      , group = Just 3
      }
    , { bodyweight = 7.1
      , supplierId = "f4"
      , damId = "d4"
      , gender = Female
      , group = Just 4
      }
    ]

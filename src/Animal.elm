module Animal exposing (..)


type Sex
    = Male
    | Female


type alias Animal =
    { bodyweight : Float
    , supplierId : String
    , damId : String
    , sex : Sex
    , group : Maybe Int
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


starterAnimals =
    [ { bodyweight = 1.1
      , supplierId = "m1"
      , damId = "d1"
      , sex = Male
      , group = Just 1
      }
    , { bodyweight = 2.1
      , supplierId = "m2"
      , damId = "d2"
      , sex = Male
      , group = Just 2
      }
    , { bodyweight = 3.1
      , supplierId = "m3"
      , damId = "d3"
      , sex = Male
      , group = Just 3
      }
    , { bodyweight = 4.1
      , supplierId = "m4"
      , damId = "d4"
      , sex = Male
      , group = Just 4
      }
    , { bodyweight = 5.1
      , supplierId = "f1"
      , damId = "d1"
      , sex = Female
      , group = Just 1
      }
    , { bodyweight = 6.1
      , supplierId = "f2"
      , damId = "d2"
      , sex = Female
      , group = Just 2
      }
    , { bodyweight = 7.1
      , supplierId = "f3"
      , damId = "d3"
      , sex = Female
      , group = Just 3
      }
    , { bodyweight = 7.1
      , supplierId = "f4"
      , damId = "d4"
      , sex = Female
      , group = Just 4
      }
    ]

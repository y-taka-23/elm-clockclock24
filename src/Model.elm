module Model exposing
    ( Clock
    , Digit
    , Display
    , Model
    , Msg(..)
    , Transition
    , TransitionStyle
    , init
    , posixToDisplay
    )

import List.Extra as List
import Time


type Msg
    = NewFrame Time.Posix


type alias Model =
    { displayed : Display
    , transition : Maybe Transition
    }


init : Model
init =
    { displayed = noDigits
    , transition = Nothing
    }


type alias Clock =
    { hour : Float
    , minute : Float
    }


blank : Clock
blank =
    { hour = 225
    , minute = 225
    }


vertical : Clock
vertical =
    { hour = 180
    , minute = 0
    }


down : Clock
down =
    { hour = 180
    , minute = 180
    }


up : Clock
up =
    { hour = 0
    , minute = 0
    }


right : Clock
right =
    { hour = 90
    , minute = 90
    }


left : Clock
left =
    { hour = 270
    , minute = 270
    }


downRight : Clock
downRight =
    { hour = 180
    , minute = 90
    }


upRight : Clock
upRight =
    { hour = 0
    , minute = 90
    }


downLeft : Clock
downLeft =
    { hour = 180
    , minute = 270
    }


upLeft : Clock
upLeft =
    { hour = 0
    , minute = 270
    }


type alias Digit =
    { topLeft : Clock
    , middleLeft : Clock
    , bottomLeft : Clock
    , topRight : Clock
    , middleRight : Clock
    , bottomRight : Clock
    }


digits : List Digit
digits =
    [ { topLeft = downRight
      , middleLeft = vertical
      , bottomLeft = upRight
      , topRight = downLeft
      , middleRight = vertical
      , bottomRight = upLeft
      }
    , { topLeft = blank
      , middleLeft = blank
      , bottomLeft = blank
      , topRight = down
      , middleRight = vertical
      , bottomRight = up
      }
    , { topLeft = right
      , middleLeft = downRight
      , bottomLeft = upRight
      , topRight = downLeft
      , middleRight = upLeft
      , bottomRight = left
      }
    , { topLeft = right
      , middleLeft = right
      , bottomLeft = right
      , topRight = downLeft
      , middleRight = upLeft
      , bottomRight = upLeft
      }
    , { topLeft = down
      , middleLeft = upRight
      , bottomLeft = blank
      , topRight = down
      , middleRight = vertical
      , bottomRight = up
      }
    , { topLeft = downRight
      , middleLeft = upRight
      , bottomLeft = right
      , topRight = left
      , middleRight = downLeft
      , bottomRight = upLeft
      }
    , { topLeft = downRight
      , middleLeft = vertical
      , bottomLeft = upRight
      , topRight = left
      , middleRight = downLeft
      , bottomRight = upLeft
      }
    , { topLeft = right
      , middleLeft = blank
      , bottomLeft = blank
      , topRight = downLeft
      , middleRight = vertical
      , bottomRight = up
      }
    , { topLeft = downRight
      , middleLeft = upRight
      , bottomLeft = upRight
      , topRight = downLeft
      , middleRight = upLeft
      , bottomRight = upLeft
      }
    , { topLeft = downRight
      , middleLeft = upRight
      , bottomLeft = right
      , topRight = downLeft
      , middleRight = vertical
      , bottomRight = upLeft
      }
    ]


unknown : Digit
unknown =
    { topLeft = blank
    , middleLeft = blank
    , bottomLeft = blank
    , topRight = blank
    , middleRight = blank
    , bottomRight = blank
    }


toDigit : Int -> Digit
toDigit n =
    Maybe.withDefault unknown <| List.getAt n digits


type alias Display =
    { d1 : Digit
    , d2 : Digit
    , d3 : Digit
    , d4 : Digit
    }


noDigits : Display
noDigits =
    { d1 = unknown
    , d2 = unknown
    , d3 = unknown
    , d4 = unknown
    }


posixToDisplay : Time.Posix -> Display
posixToDisplay _ =
    noDigits


type alias Transition =
    { style : TransitionStyle
    , from : Display
    , to : Display
    }


type alias TransitionStyle =
    { startAt : Time.Posix
    , endAt : Time.Posix
    , easing : Easing
    , hourDir : Direction
    , minuteDir : Direction
    , hourRot : Int
    , minuteRot : Int
    }


type alias Easing =
    Float -> Float


squareInOut : Easing
squareInOut x =
    if x < 1 / 2 then
        2 * x ^ 2

    else
        1 - 2 * (1 - x) ^ 2


type alias Direction =
    Float


cw : Direction
cw =
    1


ccw : Direction
ccw =
    -1

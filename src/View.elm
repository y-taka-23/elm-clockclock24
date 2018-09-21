module View exposing (view)

import Html exposing (..)
import Model exposing (Clock, Digit, Display, Model, Msg)
import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Model -> Html Msg
view model =
    div [ id "display-frame" ]
        [ display { x = 0, y = 0, padding = 4, radius = 80 } model.displayed
        ]


type alias ClockPosition =
    { centerX : Int
    , centerY : Int
    , radius : Int
    }


clock : ClockPosition -> Clock -> Svg msg
clock pos cl =
    g []
        [ dial pos
        , hourHand pos cl
        , minuteHand pos cl
        , centerPoint pos
        ]


dial : ClockPosition -> Svg msg
dial pos =
    circle
        [ class "clock-dial"
        , cx <| String.fromInt pos.centerX
        , cy <| String.fromInt pos.centerY
        , r <| String.fromInt pos.radius
        ]
        []


centerPoint : ClockPosition -> Svg msg
centerPoint pos =
    circle
        [ class "clock-center-point"
        , cx <| String.fromInt pos.centerX
        , cy <| String.fromInt pos.centerY
        ]
        []


hourHand : ClockPosition -> Clock -> Svg msg
hourHand pos cl =
    let
        l =
            toFloat pos.radius * 0.85

        deg =
            degrees cl.hour
    in
    line
        [ class "clock-hour-hand"
        , x1 <| String.fromInt pos.centerX
        , y1 <| String.fromInt pos.centerY
        , x2 <| String.fromFloat <| toFloat pos.centerX + l * sin deg
        , y2 <| String.fromFloat <| toFloat pos.centerY - l * cos deg
        ]
        []


minuteHand : ClockPosition -> Clock -> Svg msg
minuteHand pos cl =
    let
        l =
            toFloat pos.radius * 0.95

        deg =
            degrees cl.minute
    in
    line
        [ class "clock-minute-hand"
        , x1 <| String.fromInt pos.centerX
        , y1 <| String.fromInt pos.centerY
        , x2 <| String.fromFloat <| toFloat pos.centerX + l * sin deg
        , y2 <| String.fromFloat <| toFloat pos.centerY - l * cos deg
        ]
        []


type alias DigitPosition =
    { x : Int
    , y : Int
    , padding : Int
    , radius : Int
    }


digit : DigitPosition -> Digit -> Svg msg
digit pos d =
    let
        offset =
            pos.radius + pos.padding
    in
    g []
        [ clock
            { centerX = pos.x + offset
            , centerY = pos.y + offset
            , radius = pos.radius
            }
            d.topLeft
        , clock
            { centerX = pos.x + offset
            , centerY = pos.y + 3 * offset
            , radius = pos.radius
            }
            d.middleLeft
        , clock
            { centerX = pos.x + offset
            , centerY = pos.y + 5 * offset
            , radius = pos.radius
            }
            d.bottomLeft
        , clock
            { centerX = pos.x + 3 * offset
            , centerY = pos.y + offset
            , radius = pos.radius
            }
            d.topRight
        , clock
            { centerX = pos.x + 3 * offset
            , centerY = pos.y + 3 * offset
            , radius = pos.radius
            }
            d.middleRight
        , clock
            { centerX = pos.x + 3 * offset
            , centerY = pos.y + 5 * offset
            , radius = pos.radius
            }
            d.bottomRight
        ]


display : DigitPosition -> Display -> Svg msg
display pos dis =
    let
        offset =
            2 * pos.radius + 2 * pos.padding
    in
    svg
        [ id "display"
        , viewBox <|
            String.join " " <|
                List.map String.fromInt [ 0, 0, 8 * offset, 3 * offset ]
        ]
        [ digit { pos | x = 0, y = 0 } dis.d1
        , digit { pos | x = 2 * offset, y = 0 } dis.d2
        , digit { pos | x = 4 * offset, y = 0 } dis.d3
        , digit { pos | x = 6 * offset, y = 0 } dis.d4
        ]

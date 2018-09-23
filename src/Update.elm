module Update exposing (update)

import Model
    exposing
        ( Clock
        , Digit
        , Display
        , Model
        , Msg(..)
        , Transition
        , TransitionStyle
        , ccw
        , cw
        , posixToDisplay
        )
import Time
import Time.Extra as Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSystemTime ( zone, time ) ->
            ( { model
                | zone = zone
                , displayed = posixToDisplay zone time
              }
            , Cmd.none
            )

        NewFrame time ->
            case model.transition of
                Nothing ->
                    if Time.toSecond model.zone time > 40 then
                        ( { model
                            | displayed = posixToDisplay model.zone time
                            , transition = Just <| defaultMove model.zone time
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | displayed = posixToDisplay model.zone time }
                        , Cmd.none
                        )

                Just tr ->
                    let
                        current =
                            Time.posixToMillis time
                    in
                    if current < Time.posixToMillis tr.style.startAt then
                        ( { model | displayed = posixToDisplay model.zone time }
                        , Cmd.none
                        )

                    else if Time.posixToMillis tr.style.endAt <= current then
                        ( { model
                            | displayed = posixToDisplay model.zone time
                            , transition = Nothing
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | displayed = inbetween tr time }, Cmd.none )


defaultMove : Time.Zone -> Time.Posix -> Transition
defaultMove zone start =
    let
        end =
            Time.floor Time.Minute zone <| Time.add Time.Minute 1 zone start

        squareInOut x =
            if x < 1 / 2 then
                2 * x ^ 2

            else
                1 - 2 * (1 - x) ^ 2
    in
    { style =
        { startAt = start
        , endAt = end
        , easing = squareInOut
        , hourDir = cw
        , minuteDir = ccw
        , hourRot = 1
        , minuteRot = 1
        }
    , from = posixToDisplay zone start
    , to = posixToDisplay zone end
    }


inbetween : Transition -> Time.Posix -> Display
inbetween tr time =
    { d1 = inbetweenDigit tr.style tr.from.d1 tr.to.d1 time
    , d2 = inbetweenDigit tr.style tr.from.d2 tr.to.d2 time
    , d3 = inbetweenDigit tr.style tr.from.d3 tr.to.d3 time
    , d4 = inbetweenDigit tr.style tr.from.d4 tr.to.d4 time
    }


inbetweenDigit : TransitionStyle -> Digit -> Digit -> Time.Posix -> Digit
inbetweenDigit style from to time =
    { topLeft = inbetweenClock style from.topLeft to.topLeft time
    , middleLeft = inbetweenClock style from.middleLeft to.middleLeft time
    , bottomLeft = inbetweenClock style from.bottomLeft to.bottomLeft time
    , topRight = inbetweenClock style from.topRight to.topRight time
    , middleRight = inbetweenClock style from.middleRight to.middleRight time
    , bottomRight = inbetweenClock style from.bottomRight to.bottomRight time
    }


inbetweenClock : TransitionStyle -> Clock -> Clock -> Time.Posix -> Clock
inbetweenClock style from to time =
    let
        start =
            toFloat <| Time.posixToMillis style.startAt

        end =
            toFloat <| Time.posixToMillis style.endAt

        current =
            toFloat <| Time.posixToMillis time

        progress =
            style.easing <| (current - start) / (end - start)

        hourTo =
            style.hourDir * (to.hour + toFloat (style.hourRot * 360))

        minuteTo =
            style.minuteDir * (to.minute + toFloat (style.minuteRot * 360))
    in
    { hour = from.hour + progress * (hourTo - from.hour)
    , minute = from.minute + progress * (minuteTo - from.minute)
    }

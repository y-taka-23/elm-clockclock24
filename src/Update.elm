module Update exposing (update)

import Model
    exposing
        ( Clock
        , Digit
        , Direction(..)
        , Display
        , Easing
        , Model
        , Msg(..)
        , Transition
        , TransitionStyle
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
            case model.transitions of
                [] ->
                    let
                        sec =
                            Time.toSecond model.zone time
                    in
                    if 10 < sec && sec < 15 then
                        ( { model
                            | displayed = posixToDisplay model.zone time
                            , transitions = spinMove model.zone time
                          }
                        , Cmd.none
                        )

                    else if sec > 45 then
                        ( { model
                            | displayed = posixToDisplay model.zone time
                            , transitions = minuteMove model.zone time
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | displayed = posixToDisplay model.zone time }
                        , Cmd.none
                        )

                tr :: trs ->
                    let
                        current =
                            Time.posixToMillis time
                    in
                    if current < Time.posixToMillis tr.style.startAt then
                        ( model, Cmd.none )

                    else if Time.posixToMillis tr.style.endAt <= current then
                        ( { model | transitions = trs }, Cmd.none )

                    else
                        ( { model | displayed = inbetween tr time }, Cmd.none )


minuteMove : Time.Zone -> Time.Posix -> List Transition
minuteMove zone start =
    let
        end =
            Time.floor Time.Minute zone <| Time.add Time.Minute 1 zone start
    in
    [ { style =
            { startAt = start
            , endAt = end
            , easing = squareInOut
            , hourDir = CW
            , minuteDir = CCW
            , hourRot = 1
            , minuteRot = 1
            }
      , from = posixToDisplay zone start
      , to = posixToDisplay zone end
      }
    ]


spinMove : Time.Zone -> Time.Posix -> List Transition
spinMove zone start =
    let
        t1 =
            Time.add Time.Second 7 zone start

        t2 =
            Time.add Time.Second 5 zone t1

        t3 =
            Time.add Time.Second 7 zone t2

        end =
            Time.add Time.Second 7 zone t3

        align =
            { style =
                { startAt = start
                , endAt = t1
                , easing = squareInOut
                , hourDir = CCW
                , minuteDir = CW
                , hourRot = 0
                , minuteRot = 0
                }
            , from = posixToDisplay zone start
            , to = prespinPosition
            }

        prespin =
            { style =
                { startAt = t1
                , endAt = t2
                , easing = squareIn
                , hourDir = CCW
                , minuteDir = CCW
                , hourRot = 0
                , minuteRot = 0
                }
            , from = prespinPosition
            , to = spinPosition
            }

        spin =
            { style =
                { startAt = t2
                , endAt = t3
                , easing = linear
                , hourDir = CCW
                , minuteDir = CCW
                , hourRot = 1
                , minuteRot = 1
                }
            , from = spinPosition
            , to = spinPosition
            }

        recover =
            { style =
                { startAt = t3
                , endAt = end
                , easing = squareOut
                , hourDir = CCW
                , minuteDir = CCW
                , hourRot = 0
                , minuteRot = 0
                }
            , from = spinPosition
            , to = posixToDisplay zone end
            }
    in
    [ align, prespin, spin, recover ]


prespinPosition : Display
prespinPosition =
    let
        cl =
            { hour = 225, minute = 45 }

        d =
            { topLeft = cl
            , middleLeft = cl
            , bottomLeft = cl
            , topRight = cl
            , middleRight = cl
            , bottomRight = cl
            }
    in
    { d1 = d, d2 = d, d3 = d, d4 = d }


spinPosition : Display
spinPosition =
    { d1 =
        { topLeft = { hour = 45, minute = 225 }
        , middleLeft = { hour = 45, minute = 225 }
        , bottomLeft = { hour = 45, minute = 225 }
        , topRight = { hour = 60, minute = 240 }
        , middleRight = { hour = 60, minute = 240 }
        , bottomRight = { hour = 60, minute = 240 }
        }
    , d2 =
        { topLeft = { hour = 75, minute = 255 }
        , middleLeft = { hour = 75, minute = 255 }
        , bottomLeft = { hour = 75, minute = 255 }
        , topRight = { hour = 90, minute = 270 }
        , middleRight = { hour = 90, minute = 270 }
        , bottomRight = { hour = 90, minute = 270 }
        }
    , d3 =
        { topLeft = { hour = 105, minute = 285 }
        , middleLeft = { hour = 105, minute = 285 }
        , bottomLeft = { hour = 105, minute = 285 }
        , topRight = { hour = 120, minute = 300 }
        , middleRight = { hour = 120, minute = 300 }
        , bottomRight = { hour = 120, minute = 300 }
        }
    , d4 =
        { topLeft = { hour = 135, minute = 315 }
        , middleLeft = { hour = 135, minute = 315 }
        , bottomLeft = { hour = 135, minute = 315 }
        , topRight = { hour = 150, minute = 330 }
        , middleRight = { hour = 150, minute = 330 }
        , bottomRight = { hour = 150, minute = 330 }
        }
    }


squareInOut : Easing
squareInOut x =
    if x < 1 / 2 then
        2 * (x ^ 2)

    else
        1 - 2 * ((1 - x) ^ 2)


squareIn : Easing
squareIn x =
    x ^ 2


squareOut : Easing
squareOut x =
    1 - (1 - x) ^ 2


linear : Easing
linear x =
    x


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

        ( hourFrom, hourTo ) =
            case style.hourDir of
                CW ->
                    let
                        rot =
                            if from.hour > to.hour then
                                style.hourRot + 1

                            else
                                style.hourRot
                    in
                    ( from.hour
                    , to.hour + toFloat (rot * 360)
                    )

                CCW ->
                    let
                        rot =
                            if from.hour < to.hour then
                                style.hourRot + 1

                            else
                                style.hourRot
                    in
                    ( from.hour - 360
                    , to.hour - 360 - toFloat (rot * 360)
                    )

        ( minuteFrom, minuteTo ) =
            case style.minuteDir of
                CW ->
                    let
                        rot =
                            if from.minute < to.minute then
                                style.minuteRot + 1

                            else
                                style.minuteRot
                    in
                    ( from.minute
                    , to.minute + toFloat (rot * 360)
                    )

                CCW ->
                    let
                        rot =
                            if from.minute < to.minute then
                                style.minuteRot + 1

                            else
                                style.minuteRot
                    in
                    ( from.minute - 360
                    , to.minute - 360 - toFloat (rot * 360)
                    )
    in
    { hour = hourFrom + progress * (hourTo - hourFrom)
    , minute = minuteFrom + progress * (minuteTo - minuteFrom)
    }

module Effect exposing (init, subscriptions)

import Browser.Events as Browser
import Model exposing (Model, Msg(..))
import Task
import Time


init : Cmd Msg
init =
    Task.perform SetSystemTime <| Task.map2 Tuple.pair Time.here Time.now


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.onAnimationFrame NewFrame

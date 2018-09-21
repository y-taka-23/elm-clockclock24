module Effect exposing (init, subscriptions)

import Browser.Events as Browser
import Model exposing (Model, Msg(..))


init : Cmd Msg
init =
    Cmd.none


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.onAnimationFrame NewFrame

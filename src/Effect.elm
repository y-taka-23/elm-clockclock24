module Effect exposing (init, subscriptions)

import Model exposing (Model, Msg)


init : Cmd Msg
init =
    Cmd.none


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

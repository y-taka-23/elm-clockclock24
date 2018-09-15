module View exposing (view)

import Html exposing (..)
import Model exposing (Model, Msg)


view : Model -> Html Msg
view _ =
    div [] [ text "Hello, Elm!" ]

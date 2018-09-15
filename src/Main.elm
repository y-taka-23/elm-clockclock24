module Main exposing (main)

import Browser
import Effect exposing (subscriptions)
import Model exposing (Model, Msg)
import Update exposing (update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model.init, Effect.init )

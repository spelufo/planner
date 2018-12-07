module Main exposing (..)

import Browser
import Udelar
import Decode
import Json.Decode


main =
    Browser.element
        { init = init
        , update = Udelar.update
        , subscriptions = (\model -> Sub.none)
        , view = Udelar.view
        }


init : Json.Decode.Value -> ( Udelar.Model, Cmd Udelar.Msg )
init initialData =
    let
        curriculum =
            case Json.Decode.decodeValue Decode.curriculum initialData of
                Result.Ok curriculum_ ->
                    curriculum_

                Result.Err error ->
                    Debug.todo ("Failed to parse initial data: " ++ (Debug.toString error))
    in
    ( Udelar.init curriculum
    , Cmd.none
    )
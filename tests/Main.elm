port module Main exposing (..)

import TestBoundingBox as Tests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit Tests.all


port emit : ( String, Value ) -> Cmd msg

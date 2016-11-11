module Main exposing (..)

import Test.Runner.Html exposing (run)
import Expect
import Test exposing (..)
import Fuzz exposing (..)
import Vec2
import String
import Math.Vector2 exposing (vec2)
import BoundingBox exposing (..)
import Laws


toTuples bbox =
    let
        ( bottom, top ) =
            corners bbox
    in
        ( Vec2.toTuple bottom, Vec2.toTuple top )


toRecords =
    (\( a, b ) -> ( Vec2.toRecord a, Vec2.toRecord b )) << corners


example =
    fromCorners (vec2 0 10) (vec2 20 30)


vector =
    Fuzz.map2 vec2 (Fuzz.map toFloat int) (Fuzz.map toFloat int)


bbox =
    let
        customVector =
            Fuzz.map2 vec2 (Fuzz.map toFloat int) (Fuzz.map toFloat int)
    in
        Fuzz.map2 fromCorners customVector customVector


floatBBox =
    let
        customVector =
            Fuzz.map2 vec2 float float
    in
        Fuzz.map2 fromCorners customVector customVector


{-| Get the area of a bounding box.
-}
area : BoundingBox -> Float
area bbox =
    width bbox * height bbox


boundingbox =
    describe "The BoundingBox module"
        [ describe "Laws"
            [ Laws.associativity union bbox
            , Laws.commutativity union bbox
            , Laws.idempotence insert vector bbox
            , Laws.leftIdentity scale (vec2 1 1) bbox
            , Laws.leftIdentity translate (vec2 0 0) bbox
            ]
        , describe "Constructing"
            [ test "fromCorners with correct corners"
                <| \() ->
                    fromCorners (vec2 0 0) (vec2 10 10)
                        |> toTuples
                        |> Expect.equal ( ( 0, 0 ), ( 10, 10 ) )
            , test "fromCorners with partially correct corners"
                <| \() ->
                    fromCorners (vec2 10 0) (vec2 0 10)
                        |> toTuples
                        |> Expect.equal ( ( 0, 0 ), ( 10, 10 ) )
            , test "fromCorners with incorrect corners"
                <| \() ->
                    fromCorners (vec2 10 10) (vec2 0 0)
                        |> toTuples
                        |> Expect.equal ( ( 0, 0 ), ( 10, 10 ) )
            , test "insert: works as expected"
                <| \() ->
                    fromCorners (vec2 0 0) (vec2 10 10)
                        |> insert (vec2 20 -20)
                        |> toTuples
                        |> Expect.equal ( ( 0, -20 ), ( 20, 10 ) )
            , test "union: works as expected"
                <| \() ->
                    let
                        bbox1 =
                            fromCorners (vec2 0 0) (vec2 10 10)

                        bbox2 =
                            fromCorners (vec2 2 2) (vec2 12 5)
                    in
                        union bbox1 bbox2
                            |> toTuples
                            |> Expect.equal ( ( 0, 0 ), ( 12, 10 ) )
            , test "intersection: works as expected"
                <| \() ->
                    let
                        bbox1 =
                            fromCorners (vec2 0 0) (vec2 10 10)

                        bbox2 =
                            fromCorners (vec2 5 5) (vec2 20 20)
                    in
                        intersection bbox1 bbox2
                            |> Maybe.map toTuples
                            |> Expect.equal (Just ( ( 5, 5 ), ( 10, 10 ) ))
            , fuzz (tuple ( floatBBox, floatBBox )) "area of intersection is non-negative"
                <| \( bbox1, bbox2 ) ->
                    intersection bbox1 bbox2
                        |> Maybe.map area
                        |> Maybe.withDefault 0
                        |> Expect.atLeast 0
            ]
        , describe "extractors"
            [ test "topRight: works as expected"
                <| \() ->
                    example
                        |> topRight
                        |> Math.Vector2.toTuple
                        |> Expect.equal ( 20, 30 )
            , test "topLeft: works as expected"
                <| \() ->
                    example
                        |> topLeft
                        |> Math.Vector2.toTuple
                        |> Expect.equal ( 0, 30 )
            , test "bottomRight: works as expected"
                <| \() ->
                    example
                        |> bottomRight
                        |> Math.Vector2.toTuple
                        |> Expect.equal ( 20, 10 )
            , test "bottomLeft: works as expected"
                <| \() ->
                    example
                        |> bottomLeft
                        |> Math.Vector2.toTuple
                        |> Expect.equal ( 0, 10 )
            , test "width: works as expected"
                <| \() ->
                    example
                        |> width
                        |> Expect.equal 20
            , test "height: works as expected"
                <| \() ->
                    example
                        |> width
                        |> Expect.equal 20
            , fuzz (tuple4 ( float, float, float, float )) "area is non-negative"
                <| \( a, b, c, d ) ->
                    let
                        bbox =
                            fromCorners (vec2 a b) (vec2 c d)
                    in
                        area bbox
                            |> Expect.atLeast 0
            ]
        , describe "transform"
            [ test "translate: works as expected"
                <| \() ->
                    example
                        |> translate (vec2 -5 5)
                        |> toTuples
                        |> Expect.equal ( ( -5, 15 ), ( 15, 35 ) )
            , test "scale: works as expected"
                <| \() ->
                    example
                        |> scale (vec2 2 2)
                        |> toTuples
                        |> Expect.equal ( ( 0, 20 ), ( 40, 60 ) )
            ]
        , describe "membership"
            [ fuzz (tuple ( vector, bbox )) "contains"
                <| \( point, bbox ) ->
                    let
                        ( lower, upper ) =
                            corners bbox
                                |> (\( a, b ) -> ( Vec2.toRecord a, Vec2.toRecord b ))

                        ( x, y ) =
                            Vec2.toTuple point
                    in
                        if x >= lower.x && x <= upper.x && y >= lower.y && y <= upper.y then
                            contains point bbox
                                |> Expect.true "point lies within"
                        else
                            contains point bbox
                                |> Expect.false "point lies without"
            , fuzz (tuple ( bbox, bbox )) "inside"
                <| \( inner, outer ) ->
                    let
                        ( innerLower, innerUpper ) =
                            toRecords inner

                        ( outerLower, outerUpper ) =
                            toRecords outer
                    in
                        if innerLower.x >= outerLower.x && innerUpper.x <= outerUpper.x && innerLower.y >= outerLower.y && innerUpper.y <= outerUpper.y then
                            inside inner outer
                                |> Expect.true "inner lies within"
                        else
                            inside inner outer
                                |> Expect.false "inner lies without"
            , fuzz (tuple ( vector, bbox )) "onOuterEdge"
                <| \( inner_, outer ) ->
                    let
                        inner =
                            Vec2.toRecord inner_

                        ( outerLower, outerUpper ) =
                            toRecords outer
                    in
                        if inner.x == outerLower.x || inner.x == outerUpper.x || inner.y == outerLower.y || inner.y == outerUpper.y then
                            onOuterEdge inner_ outer
                                |> Expect.true "Expected: point lies on the outer edge"
                        else
                            onOuterEdge inner_ outer
                                |> Expect.false "Expected: point does not lie on the outer edge"
            ]
        , fuzz (tuple ( bbox, bbox )) "outside"
            <| \( inner, outer ) ->
                let
                    ( innerLower, innerUpper ) =
                        toRecords inner

                    ( outerLower, outerUpper ) =
                        toRecords outer

                    predicate =
                        List.any identity
                            [ innerUpper.x <= outerLower.x
                            , innerUpper.y <= outerLower.y
                            , innerLower.x >= outerUpper.x
                            , innerLower.y >= outerUpper.y
                            ]
                in
                    if predicate then
                        outside inner outer
                            |> Expect.true "Expected inner lies without"
                    else
                        outside inner outer
                            |> Expect.false "Expected inner lies within"
        ]


main =
    run boundingbox

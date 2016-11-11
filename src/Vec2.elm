module Vec2
    exposing
        ( Vec2
        , minimal
        , maximal
        , pointwise
        , pointwiseTuple
        , fold
        , fromTuple
        , toTuple
        , fromRecord
        , toRecord
        )

{-|

Currently, this module is a wrapper for [elm-community/elm-linear-algebra](http://package.elm-lang.org:8018/packages/elm-community/elm-linear-algebra/latest).
If you already use that package, the functions in this package will just work without conversion.

The elm-linear-algebra conversion functions are re-exported for convenience if you use
some other type to represent locations in 2D space.


# Vec2
@docs Vec2

# Conversion
@docs fromTuple, toTuple, fromRecord, toRecord

# Extrema
@docs minimal, maximal

# Helpers
@docs pointwise, pointwiseTuple, fold
-}

import Math.Vector2 as Vec2
import Math.Vector2


{-| The vector type that this package uses. Currently
equivalent to `Math.Vector.Vec2` but this may change in the future.
-}
type alias Vec2 =
    Math.Vector2.Vec2


{-| Select the minimal value for each component
-}
minimal : Vec2 -> Vec2 -> Vec2
minimal =
    pointwise min


{-| Select the maximal value for each component
-}
maximal : Vec2 -> Vec2 -> Vec2
maximal =
    pointwise max


{-| Apply a function pointwise to a `Vec2`.

    maximal = pointwise max
-}
pointwise : (Float -> Float -> Float) -> Vec2 -> Vec2 -> Vec2
pointwise function vec1 vec2 =
    pointwiseTuple function vec1 vec2
        |> fromTuple


{-| Consider a 2-tuple as a 2-element list and
use foldr on it.
-}
fold : (a -> b -> b) -> b -> ( a, a ) -> b
fold f default ( x, y ) =
    f x (f y default)


{-| Apply a function pointwise, but let the result be a tuple.
Because the result is not a `Vec2`, the applied function can
have any return type, not just `Float`.
-}
pointwiseTuple : (Float -> Float -> a) -> Vec2 -> Vec2 -> ( a, a )
pointwiseTuple function vec1 vec2 =
    let
        ( v1, v2 ) =
            ( Vec2.toRecord vec1, Vec2.toRecord vec2 )
    in
        ( function v1.x v2.x, function v1.y v2.y )


{-| Convert `Vec2` to `Math.Vector2.Vec2`. Currently equal to `identity`
-}
toVec2 : Vec2 -> Math.Vector2.Vec2
toVec2 =
    identity


{-| Convert `Math.Vector2.Vec2` to `Vec2`. Currently equal to `identity`
-}
fromVec2 : Math.Vector2.Vec2 -> Vec2
fromVec2 =
    identity


{-| Convert a vector to a tuple.
-}
toTuple : Vec2 -> ( Float, Float )
toTuple =
    Vec2.toTuple


{-| Convert a tuple to a vector.
-}
fromTuple : ( Float, Float ) -> Vec2
fromTuple =
    Vec2.fromTuple


{-| Convert a vector to a record.
-}
toRecord : Vec2 -> { x : Float, y : Float }
toRecord =
    Vec2.toRecord


{-| Convert a record to a vector.
-}
fromRecord : { x : Float, y : Float } -> Vec2
fromRecord =
    Vec2.fromRecord

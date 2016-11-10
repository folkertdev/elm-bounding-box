module BoundingBox
    exposing
        ( BoundingBox
        , fromPoint
        , fromPoints
        , fromCorners
        , insert
        , insertMany
        , union
        , intersection
        , intersects
          -- getters
        , corners
        , center
        , topRight
        , topLeft
        , bottomRight
        , bottomLeft
          -- dimensions
        , width
        , height
        , area
          -- membership
        , contains
        , containsStrict
        , inside
        , insideStrict
        , outside
        , outsideStrict
          -- modify bounding boxes
        , translate
        , scale
        )

{-| A 2D BoundingBox type


**Note:** Look in the `Vec2` module documentation
how to supply points of the correct type.

# Types
@docs BoundingBox

# Construct
@docs fromCorners, fromPoint, fromPoints, insert, insertMany

# Extract
@docs corners, center, topRight, topLeft, bottomRight, bottomLeft, width, height, area

# Membership
@docs contains, inside, outside, containsStrict, insideStrict, outsideStrict

# Transform
@docs translate, scale, union, intersection, intersects
-}

import Vec2 exposing (Vec2, minimal, maximal, pointwise, pointwiseTuple, fold)
import Math.Vector2 as Vec2 exposing (vec2, getX, getY)
import Math.Vector2


{-| A bounding box is defined by two points: a
lower and an upper corner.
-}
type BoundingBox
    = BoundingBox Vec2 Vec2


{-| Construct a bounding box from two vectors

**Note:** `corner1 <= corner2` doesn't need to be true:
The lower and upper corners are calculated from the four given
extremes.

    fromCorners (vec2 0 100) (vec2 20 40)
        |> corners
        -- == ( vec2 0 40, vec2 20 100)

This way a BoundingBox is always valid (i.e. its
height and width are positive values).
-}
fromCorners : Vec2 -> Vec2 -> BoundingBox
fromCorners corner =
    flip insert (fromPoint corner)


{-| Get the lower and upper corner in a tuple
-}
corners : BoundingBox -> ( Vec2, Vec2 )
corners (BoundingBox bottom top) =
    ( bottom, top )


{-| Construct a bounding box from a single vertex

    fromPoint (vec2 20 40)
        |> corners
        -- == ( vec2 20 40, vec2 20 40 )
-}
fromPoint : Vec2 -> BoundingBox
fromPoint vec =
    BoundingBox vec vec


{-| Construct a bounding box from a list of vertices.
This function needs to return maybe because the input list may
be empty.
-}
fromPoints : List Vec2 -> Maybe BoundingBox
fromPoints vecs =
    case vecs of
        v :: vs ->
            Just (List.foldr insert (fromPoint v) vs)

        [] ->
            Nothing


{-| Get the top-right corner of a bounding box

    fromCorners (vec2 0 10) (vec2 20 30)
        |> topRight
        -- == vec2 20 30
-}
topRight : BoundingBox -> Vec2
topRight (BoundingBox _ top) =
    top


{-| Get the top-left corner of a bounding box

    fromCorners (vec2 0 10) (vec2 20 30)
        |> topLeft
        -- == vec2 0 30
-}
topLeft : BoundingBox -> Vec2
topLeft (BoundingBox bottom top) =
    vec2 (getX bottom) (getY top)


{-| Get the bottom-left corner of a bounding box

    fromCorners (vec2 0 10) (vec2 20 30)
        |> bottomLeft
        -- == vec2 0 10
-}
bottomLeft : BoundingBox -> Vec2
bottomLeft (BoundingBox bottom _) =
    bottom


{-| Get the bottom-right corner of a bounding box

    fromCorners (vec2 0 10) (vec2 20 30)
        |> bottomLeft
        -- == vec2 20 10
-}
bottomRight : BoundingBox -> Vec2
bottomRight (BoundingBox bottom top) =
    vec2 (getX top) (getY bottom)


{-| Get the center of a bounding box

    fromCorners (vec2 0 0) (vec2 10 10)
        |> center
        -- == vec2 5 5
-}
center : BoundingBox -> Vec2
center (BoundingBox bottom top) =
    Vec2.scale 0.5 (Vec2.add bottom top)


{-| Get the width of a bounding box
-}
width : BoundingBox -> Float
width =
    uncurry (flip (-)) << tuple2MapBoth getX


{-| Get the width of a bounding box
-}
height : BoundingBox -> Float
height =
    uncurry (flip (-)) << tuple2MapBoth getY


{-| Get the area of a bounding box.
-}
area : BoundingBox -> Float
area bbox =
    width bbox * height bbox


{-| Check whether a point lies within a bounding box

    bbox = fromCorners (vec2 0 0) (vec2 10 10)

    contains (vec2  5  5) bbox == True
    contains (vec2 10 10) bbox == True
    contains (vec2 20 10) bbox == False
-}
contains : Vec2 -> BoundingBox -> Bool
contains p b =
    let
        check ( l, h ) =
            and (pointwiseTuple (<=) l p) && and (pointwiseTuple (<=) p h)
    in
        check (corners b)


{-| Check whether a point lies strictly within a bounding box

    bbox = fromCorners (vec2 0 0) (vec2 10 10)

    containsStrict (vec2  5  5) bbox == True
    containsStrict (vec2 10 10) bbox == False
    containsStrict (vec2 20 10) bbox == False
-}
containsStrict : Vec2 -> BoundingBox -> Bool
containsStrict p b =
    let
        check ( l, h ) =
            and (pointwiseTuple (<) l p) && and (pointwiseTuple (<) p h)
    in
        check (corners b)


{-| Check whether the first bounding box is contained by the second.

The boxes may still intersect at their boundaries

    empty = fromCorners (vec2 0 0) (vec2 0 0)
    other = fromCorners (vec2 0 0) (vec2 10 10)

    inside empty empty == True
    inside empty other == True
    inside other empty == False
-}
inside : BoundingBox -> BoundingBox -> Bool
inside u v =
    -- a bounding box lies inside another if both its corners lie inside it.
    corners u
        |> mapBoth (flip contains v)
        |> uncurry (&&)


{-| Check whether the first bounding box is strictly contained by the second.

    empty = fromCorners (vec2 0 0) (vec2 0 0)
    other = fromCorners (vec2 0 0) (vec2 10 10)

    inside empty empty == False
    inside empty other == False
    inside other empty == False
-}
insideStrict : BoundingBox -> BoundingBox -> Bool
insideStrict u v =
    -- a bounding box lies inside another if both its corners lie inside it.
    corners u
        |> mapBoth (flip containsStrict v)
        |> uncurry (&&)


{-| Check whether the first bounding box lies outside of the second.

The boxes may still intersect at their boundaries
-}
outside : BoundingBox -> BoundingBox -> Bool
outside inner outer =
    let
        ( innerLower, innerUpper ) =
            toRecords inner

        ( outerLower, outerUpper ) =
            toRecords outer
    in
        List.any identity
            [ innerUpper.x <= outerLower.x
            , innerUpper.y <= outerLower.y
            , innerLower.x >= outerUpper.x
            , innerLower.y >= outerUpper.y
            ]


{-| Check whether the first bounding box lies strictly outside of the second.
-}
outsideStrict : BoundingBox -> BoundingBox -> Bool
outsideStrict inner outer =
    let
        ( innerLower, innerUpper ) =
            toRecords inner

        ( outerLower, outerUpper ) =
            toRecords outer
    in
        List.any identity
            [ innerUpper.x < outerLower.x
            , innerUpper.y < outerLower.y
            , innerLower.x > outerUpper.x
            , innerLower.y > outerUpper.y
            ]


{-| Extend a bounding box to include a vector. If
the vector already lies within the bounding box, nothing changes.


    bbox = fromCorners (vec2 0 0) (vec2 10 10)

    insert (vec2 20 20) bbox
        |> corners
        -- == ( vec2 0 0, vec2 20 20 )
-}
insert : Vec2 -> BoundingBox -> BoundingBox
insert vec (BoundingBox bottom top) =
    BoundingBox (minimal bottom vec) (maximal top vec)


{-| Extend a bounding box with a list of vertices.
-}
insertMany : List Vec2 -> BoundingBox -> BoundingBox
insertMany points base =
    List.foldr insert base points


{-| Combine two bounding boxes into one.

    bbox1 = fromCorners (vec 0 0) (vec 10 10)
    bbox2 = fromCorners (vec 2 2) (vec 12  5)

    union bbox1 bbox2
        |> corners
        -- == ( vec2 0 0, vec2 12 10 )
-}
union : BoundingBox -> BoundingBox -> BoundingBox
union (BoundingBox bottom1 top1) (BoundingBox bottom2 top2) =
    BoundingBox (minimal bottom1 bottom2) (maximal top1 top2)


{-| Check whether two bounding boxes have at least one point in common.
-}
intersects : BoundingBox -> BoundingBox -> Bool
intersects one other =
    List.any (flip contains one) [ topLeft other, topRight other, bottomLeft other, bottomRight other ]


{-| Just the overlapping area between two bounding boxes if there is one, otherwise Nothing.
-}
intersection : BoundingBox -> BoundingBox -> Maybe BoundingBox
intersection one other =
    let
        ( oneLower, oneUpper ) =
            corners one

        ( otherLower, otherUpper ) =
            corners other
    in
        if intersects one other then
            fromCorners (maximal oneLower otherLower) (minimal oneUpper otherUpper)
                |> Just
        else
            Nothing


{-| Translate a bounding box by a vector.

    fromCorners (vec2 0 0) (vec2 10 10)
        |> translate (vec2 5 -5)
        |> corners
        -- == ( vec2 5 -5, vec2 15 5 )
-}
translate : Vec2 -> BoundingBox -> BoundingBox
translate vec =
    uncurry fromCorners << mapBoth (Vec2.add vec) << corners


{-| Scale a bounding box by a factor

    fromCorners (vec2 0 0) (vec2 10 10)
        |> scale 2
        |> corners
        -- == ( vec2 0 0, vec2 20 20 )
-}
scale : Float -> BoundingBox -> BoundingBox
scale factor =
    uncurry fromCorners << mapBoth (Vec2.scale factor) << corners



-- HELPERS


tuple2Map : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
tuple2Map f ( x1, y1 ) ( x2, y2 ) =
    ( f x1 x2, f y1 y2 )


tuple2MapBoth : (Vec2 -> a) -> BoundingBox -> ( a, a )
tuple2MapBoth f (BoundingBox bottom top) =
    ( f bottom, f top )


mapBoth : (a -> b) -> ( a, a ) -> ( b, b )
mapBoth f ( a, b ) =
    ( f a, f b )


and : ( Bool, Bool ) -> Bool
and =
    uncurry (&&)


toTuples : BoundingBox -> ( ( Float, Float ), ( Float, Float ) )
toTuples bbox =
    let
        ( bottom, top ) =
            corners bbox
    in
        ( Math.Vector2.toTuple bottom, Math.Vector2.toTuple top )


toRecords : BoundingBox -> ( { x : Float, y : Float }, { x : Float, y : Float } )
toRecords =
    (\( a, b ) -> ( Math.Vector2.toRecord a, Math.Vector2.toRecord b )) << corners

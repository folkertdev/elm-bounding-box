module Main exposing (..)

{-| Aim: calculate the minimum size that a svg element needs to have to correctly display our content
-}

import Math.Vector2 exposing (vec2, getX, getY)
import Vec2 exposing (Vec2)
import BoundingBox exposing (fromCorners, width, height)
import Svg exposing (Svg)
import Svg.Attributes exposing (stroke, strokeWidth, points, fill)
import String
import Tuple exposing (first, second)


type alias Settings =
    { margins : ( Float, Float )
    , defaultDimensions : ( Float, Float )
    }


fitBoxToPoints : Settings -> List Vec2 -> List (Svg msg) -> Svg msg
fitBoxToPoints settings vectors contents =
    let
        ( marginX, marginY ) =
            settings.margins

        dimensions =
            BoundingBox.fromPoints vectors
                |> Maybe.map (\bbox -> ( width bbox, height bbox ))
                |> Maybe.withDefault settings.defaultDimensions

        offsets =
            ( getX offset - marginX
            , getX offset / 2 - marginY
            )

        viewbox w h =
            [ first offsets
            , second offsets
            , w + marginX
            , h + marginY + marginY
            ]
                |> List.map toString
                |> String.join " "

        offset =
            List.head vectors
                |> Maybe.withDefault (vec2 0 0)
    in
        Svg.svg
            [ Svg.Attributes.viewBox (uncurry viewbox dimensions)
            , Svg.Attributes.width (toString (first dimensions + first offsets))
            , Svg.Attributes.height (toString (marginY + second dimensions + second offsets))
            ]
            contents


main =
    let
        settings =
            { margins = ( 0, 10 )
            , defaultDimensions = ( 100, 100 )
            }

        content =
            [ Svg.polyline
                [ stroke "blue"
                , strokeWidth "20"
                , fill "none"
                , vectors
                    |> List.map vec2string
                    |> String.join " "
                    |> points
                ]
                []
            ]
    in
        fitBoxToPoints settings vectors content


vec2string vec =
    toString (getX vec) ++ "," ++ toString (getY vec)


vectors =
    [ vec2 50 375
    , vec2 150 375
    , vec2 150 325
    , vec2 250 325
    , vec2 250 375
    , vec2 350 375
    , vec2 350 250
    , vec2 450 250
    , vec2 450 375
    , vec2 550 375
    , vec2 550 175
    , vec2 650 175
    , vec2 650 375
    , vec2 750 375
    , vec2 750 100
    , vec2 850 100
    , vec2 850 375
    , vec2 950 375
    , vec2 950 25
    , vec2 1050 25
    , vec2 1050 375
    , vec2 1150 375
    ]

module Test exposing (..)


import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Plot exposing (..)
import Html exposing (..)
import Html.Attributes exposing (href,style)

main =
    viewSeries
        [ line (List.map (\{ x, y } -> circle x y)) ]
        [ { x = 0, y = 1 }
        , { x = 2, y = 2 }
        , { x = 3, y = 3 }
        , { x = 4, y = 5 }
        , { x = 5, y = 8 }
        ]
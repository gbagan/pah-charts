module Test.Main where

import Prelude

import Effect (Effect)
import Pha.App (sandbox)
import Pha.Html as H
import Pha.Chart as C
import Pha.Chart.Attributes as CA

main :: Effect Unit
main =
  sandbox
  { init: unit
  , update: \_ x -> x
  , view: \_ ->
      H.div [] $
        [ C.chart
          [ CA.height 300.0
          , CA.width 300.0
          ]
            [ C.xLabels [ CA.withGrid ]
            , C.yLabels [ CA.withGrid ]
            , C.series _.x
              [ C.stacked
                [ C.interpolated _.y [ CA.opacity 0.2 ] []
                , C.interpolated _.z [ CA.opacity 1.0, CA.dotted [] ] []
                ]
              ]
              [ { x: 1.0, y: 1.0, z: 3.0 }
              , { x: 5.0, y: 2.0, z: 1.0 }
              , { x: 10.0, y: 2.0, z: 4.0 }
              ]
            ]
        ]
  , selector: "#root"
  }

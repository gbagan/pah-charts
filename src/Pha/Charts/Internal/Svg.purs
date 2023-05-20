module Pha.Charts.Internal.Svg where

import Prelude

import Data.Int as Int
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (px)
import Pha.Charts.Internal.Coordinates (Plane, Point)
import Pha.Charts.Internal.Coordinates as Coord
import Pha.Charts.Internal.Commands (Command(..))
import Pha.Charts.Internal.Commands as C
import Pha.Charts.Internal.Interpolation as Interpolation
import Pha.Charts.Internal.Helpers as Helpers

type Container msg =
  { attrs :: Array (H.Prop msg)
  , htmlAttrs :: Array (H.Prop msg)
  , responsive :: Boolean
  , events :: Array (Event msg)
  }

type Event msg =
  { name :: String
  , handler :: Plane -> Point -> msg
  }


defaultContainer :: forall msg. Container msg
defaultContainer =
  { attrs: [ H.style "overflow" "visible" ]
  , htmlAttrs: []
  , responsive: true
  , events: []
  }

container :: forall msg. Plane -> Container msg -> Array (Html msg) -> Array (Html msg) -> Array (Html msg) -> Html msg
container plane config below chartEls above =
  H.div
    [ H.class_ "elm-charts__container"
    , H.style "position" "relative"
    ]
    [ H.div htmlAttrs (below <> [ chart ] <> above) ]
  where
  -- TODO seperate plane from container size
  -- TODO preserveAspectRatio?
  htmlAttrsDef = [ H.class_ "elm-charts__container-inner" ]

  htmlAttrsSize =
        if config.responsive then
          [ H.style "width" "100%"
          , H.style "height" "100%"
          ]
        else
          [ H.style "width" (px plane.x.length)
          , H.style "height" (px plane.y.length)
          ]

  htmlAttrs = config.htmlAttrs <> htmlAttrsDef <> htmlAttrsSize

  chart = H.svg
          (svgAttrsSize <> config.attrs)
          ([clipPathDefs] <> chartEls <> [catcher])

  svgAttrsSize =
        if config.responsive then
          [ P.viewBox 0 0 (Int.ceil plane.x.length) (Int.ceil plane.y.length)
          , H.style "display" "block"
          ]
        else
          [ P.width plane.x.length
          , P.height plane.y.length
          , H.style "display" "block"
          ]

  catcher = H.rect (chartPosition) -- ++ map toEvent config.events)

  -- toEvent event = SE.on event.name (decoder plane event.handler)

  chartPosition =
    [ P.x plane.x.marginMin
    , P.y plane.y.marginMin
    , P.width $ Coord.innerWidth plane
    , P.height $ Coord.innerHeight plane
    , P.fill "transparent"
    ]

  clipPathDefs =
    H.defs []
      [ H.elem "clipPath"
        [ P.id (Coord.toId plane) ]
        [ H.rect chartPosition ]
      ]

data TickType
  = Floats
  | Ints
  -- | Times Time.Zone

type Tick =
  { color :: String
  , width :: Number
  , length :: Number
  , attrs :: Array (H.Prop Void)
  }

defaultTick :: Tick
defaultTick =
  { length: 5.0
  , color: "rgb(210, 210, 210)"
  , width: 1.0
  , attrs: []
  }

xTick :: Plane -> Tick -> Point -> Html msg
xTick plane config point = tick plane config true point

yTick :: Plane -> Tick -> Point -> Html msg
yTick plane config point = tick plane config false point

tick :: Plane -> Tick -> Boolean -> Point -> Html msg
tick plane config isX point =
  withAttrs config.attrs H.line
    [ H.class_ "elm-charts__tick"
    , P.stroke config.color
    , P.strokeWidth config.width
    , P.x1 $ Coord.toSVGX plane point.x
    , P.x2 $ Coord.toSVGX plane point.x + if isX then 0 else -config.length
    , P.y1 $ Coord.toSVGY plane point.y
    , P.y2 $ Coord.toSVGY plane point.y + if isX then config.length else 0
    ]



-- HELPERS


withAttrs :: forall msg
            . Array (H.Prop Void)
            -> (Array (H.Prop msg) -> Array (Html msg) -> Html msg)
            -> Array (H.Prop msg) 
            -> Array (Html msg) 
            -> Html msg
withAttrs attrs toEl defaultAttrs =
  toEl (defaultAttrs <> map (map absurd) attrs)
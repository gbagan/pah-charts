module Pha.Charts.Svg where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Pha.Html (Html)
import Pha.Charts.Attributes as CA
import Pha.Charts.Internal.Coordinates (Plane, Point)
import Pha.Charts.Internal.Helpers (applyFuncs)
import Pha.Charts.Internal.Svg (Bar, Dot, Interpolation, Legend, Legends, Line, Rect, Tick)
import Pha.Charts.Internal.Svg as IS

xTick :: forall msg. Plane -> Array (CA.Attribute Tick) -> Point -> Html msg
xTick plane edits =
  IS.xTick plane (applyFuncs edits IS.defaultTick)


yTick :: forall msg. Plane -> Array (CA.Attribute Tick) -> Point -> Html msg
yTick plane edits = IS.yTick plane (applyFuncs edits IS.defaultTick)


tick :: forall msg. Plane -> Array (CA.Attribute Tick) -> Boolean -> Point -> Html msg
tick plane edits = IS.tick plane (applyFuncs edits IS.defaultTick)

line :: forall msg. Plane -> Array (CA.Attribute Line) -> Html msg
line plane edits = IS.line plane (applyFuncs edits IS.defaultLine)

rect :: forall msg. Plane -> Array (CA.Attribute Rect) -> Html msg
rect plane edits = IS.rect plane (applyFuncs edits IS.defaultRect)

legendsAt :: forall msg. Plane -> Number -> Number -> Array (CA.Attribute (Legends msg)) -> Array (Html msg) -> Html msg
legendsAt plane x y edits = IS.legendsAt plane x y (applyFuncs edits IS.defaultLegends)

barLegend :: forall msg. Array (CA.Attribute (Legend msg)) -> Array (Bar -> Bar) -> Html msg
barLegend edits barAttrs =
  IS.barLegend
    (applyFuncs edits IS.defaultBarLegend)
    (applyFuncs barAttrs IS.defaultBar)

lineLegend :: forall msg
              . Array (CA.Attribute (Legend msg)) 
              -> Array (CA.Attribute Interpolation) 
              -> Array (CA.Attribute Dot) 
              -> Html msg
lineLegend edits interAttrsOrg dotAttrsOrg =
  IS.lineLegend
    (applyFuncs lineLegendAttrs IS.defaultLineLegend)
    (applyFuncs interAttrs IS.defaultInterpolation)
    (applyFuncs dotAttrs IS.defaultDot)
  where

  interpolationConfigOrg = applyFuncs interAttrsOrg IS.defaultInterpolation
  dotConfigOrg = applyFuncs dotAttrsOrg IS.defaultDot

  dotAttrs /\ interAttrs /\ lineLegendAttrs =
    case interpolationConfigOrg.method, dotConfigOrg.shape of
      Just _, Nothing  -> dotAttrsOrg /\ interAttrsOrg /\ ([CA.width 10.0] <> edits)
      Nothing, Nothing -> ([CA.circle] <> dotAttrsOrg) /\ ([CA.linear] <> interAttrsOrg) /\ ([CA.width 10.0] <> edits)
      Nothing, Just _  -> ([CA.circle] <> dotAttrsOrg) /\ interAttrsOrg /\ ([CA.width 10.0] <> edits)
      _, _             -> dotAttrsOrg /\ ([CA.opacity 0.0] <> interAttrsOrg) /\ edits

  -- adjustWidth config = config {width = 10.0}
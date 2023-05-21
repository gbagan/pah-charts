module Pha.Chart.Internal.Legend where

import Prelude
import Data.Array (length, mapWithIndex)
import Data.Maybe (fromMaybe, isNothing)
import Pha.Html as H
import Pha.Html (Html)
import Pha.Html.Attributes as P
import Pha.Chart.Internal.Coordinates (Point, Position, Plane)
import Pha.Chart.Internal.Coordinates as Coord
import Pha.Chart.Internal.Property as IP 
import Pha.Chart.Internal.Property (Property)
import Pha.Chart.Internal.Svg as S
import Pha.Chart.Attributes as CA
import Pha.Chart.Internal.Helpers (applyFuncs, toDefaultColor)
import Pha.Chart.Internal.Produce as Produce


data Legend
  = BarLegend String (Array (CA.Attribute S.Bar))
  | LineLegend String (Array (CA.Attribute S.Interpolation)) (Array (CA.Attribute S.Dot))


toBarLegends :: forall data_
                . Int 
                -> Array (CA.Attribute (Produce.Bars data_)) 
                -> Array (Property data_ String Unit S.Bar) 
                -> Array Legend
toBarLegends elIndex barsAttrs properties =
  properties 
    >>= IP.toConfigs
    # mapWithIndex (\propIndex -> toBarLegend (elIndex + propIndex))
  where
  barsConfig = applyFuncs barsAttrs Produce.defaultBars

  toBarConfig attrs = applyFuncs attrs S.defaultBar

  toBarLegend colorIndex prop =
    BarLegend (fromMaybe defaultName prop.meta) attrs
    where
    defaultName = "Property #" <> show (colorIndex + 1)
    defaultColor = toDefaultColor colorIndex
    rounding = max barsConfig.roundTop barsConfig.roundBottom
    defaultAttrs = [ CA.roundTop rounding, CA.roundBottom rounding, CA.color defaultColor, CA.border defaultColor ]
    attrsOrg = defaultAttrs <> prop.attrs
    productOrg = toBarConfig attrsOrg
    attrs = if productOrg.border == defaultColor then attrsOrg <> [ CA.border productOrg.color ] else attrsOrg        


toDotLegends :: forall data_
                . Int 
                -> Array (Property data_ String S.Interpolation S.Dot) 
                -> Array Legend
toDotLegends elIndex properties =
  map IP.toConfigs properties
    >>= (\ps -> toDotLegend ps <$> ps)
    # mapWithIndex (\propIndex f -> f (elIndex + propIndex))
  where
  toInterConfig attrs = applyFuncs attrs S.defaultInterpolation

  toDotLegend props prop colorIndex =
    LineLegend (fromMaybe defaultName prop.meta) interAttr dotAttrs
    where
    defaultOpacity = if length props > 1 then 0.4 else 0.0
    interAttr = [ CA.color (toDefaultColor colorIndex), CA.opacity defaultOpacity ] <> prop.inter
    interConfig = toInterConfig interAttr
    defaultAttrs = [ CA.color interConfig.color, CA.border interConfig.color, if isNothing interConfig.method then CA.circle else identity ]
    dotAttrs = defaultAttrs <> prop.attrs
    defaultName = "Property #" <> show (colorIndex + 1)     

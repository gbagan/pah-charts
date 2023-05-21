module Pha.Chart.Attributes where

import Prelude
import Data.Maybe (Maybe(..))
import Pha.Chart.Internal.Coordinates as C
import Pha.Chart.Internal.Svg as CS

type Attribute c = c -> c

-- LIMITS

-- | Change the lower bound of an axis.
lowest :: Number -> (Number -> Number -> Number -> Number) -> Attribute C.Axis
lowest v edit b = b { min = edit v b.min b.dataMin }

-- | Same as `lowest`, but changes upper bound.
highest :: Number -> (Number -> Number -> Number -> Number) -> Attribute C.Axis
highest v edit b = b { max = edit v b.max b.dataMax }

-- | Resets axis to fit data bounds.
likeData :: Attribute C.Axis
likeData b = b { min = b.dataMin, max = b.dataMax }

-- | Set an axis to an exact window.
window :: Number -> Number -> Attribute C.Axis
window min_ max_ b = b { min = min_, max = max_ }

-- | See `lowest` for usage examples.
exactly :: Number -> Number -> Number -> Number
exactly exact _ _ = exact

-- | See `lowest` for usage examples.
orLower :: Number -> Number -> Number -> Number
orLower least real _ =
  if real > least then least else real

-- | See `lowest` for usage examples.
orHigher :: Number -> Number -> Number -> Number
orHigher most real _ = if real < most then most else real

-- | See `lowest` for usage examples.
more :: Number -> Number -> Number -> Number
more v o _ = o + v

-- | See `lowest` for usage examples.
less :: Number -> Number -> Number -> Number
less v o _ = o - v

-- | Zoom with a certain percentage.
zoom :: Number -> Attribute C.Axis
zoom per axis = axis { min = axis.min + off, max = axis.max - off }
  where
  full = axis.max - axis.min
  zoomedFull = full / (max 1.0 per / 100.0)
  off = (full - zoomedFull) / 2.0

move :: Number -> Attribute C.Axis
move v axis = axis { min = axis.min + v, max = axis.max + v }

-- | Add padding (in px) to range/domain.

pad :: Number -> Number -> Attribute C.Axis
pad minPad maxPad axis =
  let scale = C.scaleCartesian axis in
  axis { min = axis.min - scale minPad, max = axis.max + scale maxPad }


-- | Center range/domain at certain point.
centerAt :: Number -> Attribute C.Axis
centerAt v axis =
  let full = axis.max - axis.min in
  axis { min = v - full / 2.0, max = v + full / 2.0 }


-- | Given an axis, find the value within it closest to zero.
zero :: C.Axis -> Number
zero b = clamp b.min b.max 0.0


-- | Get the middle value of your axis.
middle :: C.Axis -> Number
middle b = b.min + (b.max - b.min) / 2.0





x :: forall r. Number -> Attribute { x :: Number | r }
x v = _ { x = v }

x1 :: forall x r. x -> Attribute { x1 :: Maybe x  | r }
x1 v = _ { x1 = Just v }

x2 :: forall x r. x -> Attribute { x2 :: Maybe x | r }
x2 v = _ { x2 = Just v }

x2Svg :: forall x r. x -> Attribute { x2Svg :: Maybe x | r }
x2Svg v = _ { x2Svg = Just v }

y :: forall r. Number -> Attribute { y :: Number | r }
y v = _ { y = v }

y1 :: forall r. Number -> Attribute { y1 :: Maybe Number | r }
y1 v = _ { y1 = Just v }

y2 :: forall r. Number -> Attribute { y2 :: Maybe Number | r }
y2 v = _ { y2 = Just v }

y2Svg :: forall x r. x -> Attribute { y2Svg :: Maybe x | r }
y2Svg v  = _ { y2Svg = Just v }

-- GRID

noGrid :: forall r. Attribute { grid :: Boolean | r }
noGrid = _ { grid = false }

withGrid :: forall r. Attribute { grid :: Boolean | r }
withGrid = _ { grid = true }




border :: forall r. String -> Attribute { border :: String | r }
border v = _ { border = v }

color :: forall r. String -> Attribute { color :: String | r }
color v config = if v == "" then config else config { color = v }

roundTop :: forall r. Number -> Attribute { roundTop :: Number | r }
roundTop v = _ { roundTop = v }

roundBottom :: forall r. Number -> Attribute { roundBottom :: Number | r }
roundBottom v = _ { roundBottom = v }

width :: forall r. Number -> Attribute { width :: Number | r }
width v = _ { width = v }


height :: forall r. Number -> Attribute { height :: Number | r }
height v = _ { height = v }

-- DECORATION

opacity :: forall r. Number -> Attribute { opacity :: Number | r }
opacity v = _ { opacity = v }

dotted :: forall r
            . Array (Attribute CS.Pattern) 
            -> Attribute { design :: Maybe CS.Design, opacity :: Number | r }
dotted attrs_ config =
  config { design = Just (CS.Dotted attrs_), opacity = if config.opacity == 0.0 then 1.0 else config.opacity }



-- LINES

size :: forall r. Number -> Attribute { size :: Number | r }
size v = _ { size = v }

dashed :: forall x r. x -> Attribute { dashed :: x | r }
dashed value = _ { dashed = value }

circle :: forall r. Attribute { shape :: Maybe CS.Shape | r }
circle  = _ { shape = Just CS.Circle }


triangle :: forall r. Attribute { shape :: Maybe CS.Shape | r }
triangle = _ { shape = Just CS.Triangle }

square :: forall r. Attribute { shape :: Maybe CS.Shape | r }
square = _ { shape = Just CS.Square }

diamond :: forall r. Attribute { shape :: Maybe CS.Shape | r }
diamond = _ { shape = Just CS.Diamond }


plus :: forall r. Attribute { shape :: Maybe CS.Shape | r }
plus = _ { shape = Just CS.Plus }

cross :: forall r. Attribute { shape :: Maybe CS.Shape | r }
cross = _ { shape = Just CS.Cross }


-- FOCAL

linear :: forall r. Attribute { method :: Maybe CS.Method | r }
linear = _ { method = Just CS.Linear }
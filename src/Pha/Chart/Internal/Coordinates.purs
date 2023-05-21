module Pha.Chart.Internal.Coordinates where

import Prelude
import Data.Array (foldl, mapMaybe)
import Data.Foldable (minimum, maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), joinWith, replace)

type Point =
  { x :: Number
  , y :: Number
  }


type Position =
  { x1 :: Number
  , x2 :: Number
  , y1 :: Number
  , y2 :: Number
  }


center :: Position -> Point
center pos = { x: pos.x1 + (pos.x2 - pos.x1) / 2.0, y: pos.y1 + (pos.y2 - pos.y1) / 2.0 }

top :: Position -> Point
top pos = { x: pos.x1 + (pos.x2 - pos.x1) / 2.0, y: pos.y2 }

bottom :: Position -> Point
bottom pos = { x: pos.x1 + (pos.x2 - pos.x1) / 2.0, y: pos.y1 }

left :: Position -> Point
left pos = { x: pos.x1, y: pos.y1 + (pos.y2 - pos.y1) / 2.0 }

right :: Position -> Point
right pos = { x: pos.x2, y: pos.y1 + (pos.y2 - pos.y1) / 2.0 }

topLeft :: Position -> Point
topLeft pos = { x: pos.x1, y: pos.y2 }

topRight :: Position -> Point
topRight pos = { x: pos.x2, y: pos.y2 }

bottomLeft :: Position -> Point
bottomLeft pos = { x: pos.x1, y: pos.y1 }


bottomRight :: Position -> Point
bottomRight pos = { x: pos.x2, y: pos.y1 }


pointToPosition :: Point -> Position
pointToPosition point = { x1: point.x, x2: point.x, y1: point.y, y2: point.y }


foldPosition :: forall a. (a -> Position) -> Array a -> Position
foldPosition func data_ = foldl fold Nothing data_
    # fromMaybe {x1: 0.0, x2: 0.0, y1: 0.0, y2: 0.0} -- TODO
  where
  fold posM datum =
    let pos2 = func datum in
    case posM of
      Just pos ->
        Just
          { x1: min pos2.x1 pos.x1
          , x2: max pos2.x2 pos.x2
          , y1: min pos2.y1 pos.y1
          , y2: max pos2.y2 pos.y2
          }
      Nothing -> Just pos2

fromProps :: forall a. Array (a -> Maybe Number) -> Array (a -> Maybe Number) -> Array a -> Position
fromProps xs ys data_ = foldPosition toPosition data_
  where
  toPosition datum =
        let vsX = getValues xs datum
            vsY = getValues ys datum
        in
        { x1: getMin vsX
        , x2: getMax vsX
        , y1: getMin vsY
        , y2: getMax vsY
        }

  getMin = fromMaybe 0.0 <<< minimum
  getMax = fromMaybe 1.0 <<< maximum
  getValues vs datum = mapMaybe (\v -> v datum) vs
  



-- PLANE

type Plane =
  { x :: Axis
  , y :: Axis
  }

type Axis =
  { length :: Number
  , marginMin :: Number
  , marginMax :: Number
  , dataMin :: Number
  , dataMax :: Number
  , min :: Number
  , max :: Number
  }


type Limit =
  { min :: Number
  , max :: Number
  }



-- ID


{-| An id for the clip path. This needs to be unique for the particular dimensions
of the chart, but not necessarily for the whole document. -}
toId :: Plane -> String
toId plane =
  joinWith "_"
    [ "elm-charts__id"
    , numToStr plane.x.length
    , numToStr plane.x.min
    , numToStr plane.x.max
    , numToStr plane.x.marginMin
    , numToStr plane.x.marginMax
    , numToStr plane.y.length
    , numToStr plane.y.min
    , numToStr plane.y.max
    , numToStr plane.y.marginMin
    , numToStr plane.y.marginMax
    ]
  where
  numToStr = show >>> replace (Pattern ".") (Replacement "-")


-- TRANSLATION


-- | For scaling a cartesian value to a SVG value. Note that this will _not_
-- | return a coordinate on the plane, but the scaled value.
scaleSVGX :: Plane -> Number -> Number
scaleSVGX plane value =
  value * (innerWidth plane) / (range plane.x)

scaleSVGY :: Plane -> Number -> Number
scaleSVGY plane value = value * (innerHeight plane) / (range plane.y)

-- | Translate a SVG x-coordinate to its cartesian x-coordinate.
toSVGX :: Plane -> Number -> Number
toSVGX plane value = scaleSVGX plane (value - plane.x.min) + plane.x.marginMin


-- | Translate a SVG y-coordinate to its cartesian y-coordinate.
toSVGY :: Plane -> Number -> Number
toSVGY plane value = scaleSVGY plane (plane.y.max - value) + plane.y.marginMin


-- | For scaling a SVG value to a cartesian value. Note that this will _not_
-- | return a coordinate on the plane, but the scaled value.

scaleCartesianX :: Plane -> Number -> Number
scaleCartesianX plane value = value * (range plane.x) / (innerWidth plane)


scaleCartesianY :: Plane -> Number -> Number
scaleCartesianY plane value = value * (range plane.y) / (innerHeight plane)


scaleCartesian :: Axis -> Number -> Number
scaleCartesian axis value = value * (range axis) / (innerLength axis)


{-| Translate a cartesian x-coordinate to its SVG x-coordinate.
-}
toCartesianX :: Plane -> Number -> Number
toCartesianX plane value =
  scaleCartesianX plane (value - plane.x.marginMin) + plane.x.min


-- | Translate a cartesian y-coordinate to its SVG y-coordinate.
toCartesianY :: Plane -> Number -> Number
toCartesianY plane value =
  range plane.y - scaleCartesianY plane (value - plane.y.marginMin) + plane.y.min



-- INTERNAL HELPERS


range :: Axis -> Number
range axis =
  let diff = axis.max - axis.min in
  if diff > 0.0 then diff else 1.0


innerWidth :: Plane -> Number
innerWidth plane = innerLength plane.x

innerHeight :: Plane -> Number
innerHeight plane = innerLength plane.y

innerLength :: Axis -> Number
innerLength axis = max 1.0 (axis.length - axis.marginMin - axis.marginMax)

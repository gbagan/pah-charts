module Pha.Charts.Internal.Interpolation (linear, monotone, stepped) where

import Prelude
import Data.Array (foldr, head)
import Data.List as List
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Number (isNaN)
import Data.Ord (abs)
import Data.Tuple (Tuple, snd, uncurry)
import Data.Tuple.Nested ((/\))
import Pha.Charts.Internal.Commands (Command(..))
import Pha.Charts.Internal.Helpers (pairwise)

type Point =
  { x :: Number
  , y :: Number
  }



-- LINEAR INTERPOLATION


linear :: Array (Array Point) -> Array (Array Command)
linear = map $ map \{ x, y } -> Line x y


-- MONOTONE INTERPOLATION


monotone :: Array (Array Point) -> Array (Array Command)
monotone sections = snd $ foldr monotoneSection (First /\ []) sections


monotoneSection :: Array Point -> (Tuple Tangent (Array (Array Command))) -> (Tuple Tangent (Array (Array Command)))
monotoneSection points (tangent /\ acc ) =
  let
    (t0 /\ commands) =
      case head points of
        Nothing -> tangent /\ []
        Just p0 -> monotonePart (List.fromFoldable points) ( tangent /\ [ Line p0.x p0.y ] )
          
  in
  ( t0 /\ ([commands] <> acc))


data Tangent = First | Previous Number


monotonePart :: List Point -> (Tuple Tangent (Array Command)) -> (Tuple Tangent (Array Command))
monotonePart points (tangent /\ commands) =
  case tangent, points of
    First, p0 : p1 : p2 : rest ->
      let t1 = slope3 p0 p1 p2
          t0 = slope2 p0 p1 t1
      in
      Previous t1 /\ (commands <> [monotoneCurve p0 p1 t0 t1])
      # monotonePart (p1 : p2 : rest)

    Previous t0, (p0 : p1 : p2 : rest) ->
      let t1 = slope3 p0 p1 p2 in
      Previous t1 /\ (commands <> [monotoneCurve p0 p1 t0 t1])
      # monotonePart (p1 : p2 : rest)

    First, p0 : p1 : Nil ->
      let t1 = slope3 p0 p1 p1 in
      Previous t1 /\ (commands <> [monotoneCurve p0 p1 t1 t1, Line p1.x p1.y])

    Previous t0, p0 : p1 : Nil ->
      let t1 = slope3 p0 p1 p1 in
      Previous t1 /\ (commands  <> [monotoneCurve p0 p1 t0 t1, Line p1.x p1.y])
    
    _, _  ->  tangent /\ commands

monotoneCurve :: Point -> Point -> Number -> Number -> Command
monotoneCurve point0 point1 tangent0 tangent1 =
  CubicBeziers
      (point0.x + dx)
      (point0.y + dx * tangent0)
      (point1.x - dx)
      (point1.y - dx * tangent1)
      point1.x
      point1.y
  where
  dx = (point1.x - point0.x) / 3.0


-- | Calculate the slopes of the tangents (Hermite-type interpolation) based on
-- | the following paper: Steffen, M. 1990. A Simple Method for Monotonic
-- | Interpolation in One Dimension
slope3 :: Point -> Point -> Point -> Number
slope3 point0 point1 point2 =
  let
    h0 = point1.x - point0.x
    h1 = point2.x - point1.x
    s0h = toH h0 h1
    s1h = toH h1 h0
    s0 = (point1.y - point0.y) / s0h
    s1 = (point2.y - point1.y) / s1h
    p = (s0 * h1 + s1 * h0) / (h0 + h1)
    slope = (sign s0 + sign s1) * (min (min (abs s0) (abs s1)) (0.5 * abs p))
  in
    if isNaN slope then 0.0 else slope


toH :: Number -> Number -> Number
toH h0 h1 =
  if h0 == 0.0
    then if h1 < 0.0 then -0.0 else h1
    else h0


-- | Calculate a one-sided slope.
slope2 :: Point -> Point -> Number -> Number
slope2 point0 point1 t =
  let h = point1.x - point0.x in
    if h /= 0.0
      then (3.0 * (point1.y - point0.y) / h - t) / 2.0
      else t


sign :: Number -> Number
sign x = if x < 0.0 then -1.0 else 1.0


-- STEPPED


stepped :: Array (Array Point) -> Array (Array Command)
stepped sections =
  sections <#> (\section -> pairwise section >>= uncurry after)
           <#> map \{ x, y } -> Line x y


after :: Point -> Point -> Array Point
after a b = [a, {x: b.x, y: a.y}, b]
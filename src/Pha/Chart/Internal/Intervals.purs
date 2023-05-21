module Pha.Chart.Internal.Intervals (Amount, around, exactly, Range, ints, floats, custom) where

import Prelude
import Data.Array (filter, findMap, snoc)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Int as Int
import Data.Int (toNumber)
import Data.List as List
import Data.List (List(..), (:))
import Data.Number as Number
import Data.Number (ceil, floor, log, ln10, pow, round)
import Data.Ord (abs)
import Data.String as String
import Data.String (split, Pattern(..))
import Data.Number.Format as F

data Amount = Exactly Int | Around Int


{-| Will get you around the amount of numbers you pass it, although it will
prioritize getting "nice" numbers.
-}
around :: Int -> Amount
around = Around


{-| Will get you _closer_ to the amount of numbers you pass it,
although not actually _exactly_, since you still want decently "nice" numbers.

P.S. If you have a better name for this function, please contact me.

-}
exactly :: Int -> Amount
exactly = Exactly


-- | The upper and lower bound of your numbers/timestamps.
type Range =
  { min :: Number
  , max :: Number
  }


-- | Produce a list of "nice" integers.
ints :: Amount -> Range -> Array Int
ints amount range =
  map Int.round $
    case amount of
      Exactly number -> values false true number range
      Around number -> values false false number range

-- | Produce a list of "nice" floats.
floats :: Amount -> Range -> Array Number
floats amount =
  case amount of
    Exactly number -> values true true number
    Around number -> values true false number



{-| Makes evenly spaced floats.

Arguments:
  1. A number which must be in your resulting numbers (commonly 0).
  2. The interval between your numbers.
  3. The range which your numbers must be between.

    Intervals.custom 45 10 (Range 25 100)
    -- ^ Makes [ 25, 35, 45, 55, 65, 75, 85, 95 ]

    Intervals.custom 30 20 (Range 25 100)
    -- ^ Makes [ 30, 50, 70, 90 ]

-}
custom :: Number -> Number -> Range -> Array Number
custom intersection interval range =
  positions range beginning interval 0.0 []
  where
  offset value = interval * (floor (value / interval))

  beginning = intersection - offset (intersection - range.min)

values :: Boolean -> Boolean -> Int -> Range -> Array Number
values allowDecimals exact amountRough range =
  positions range beginning intervalSafe 0.0 []
  where
  -- amountRoughSafe = if amountRough == 0 then 1 else amountRough

  intervalRough = (range.max - range.min) / toNumber amountRough

  interval = getInterval intervalRough allowDecimals exact

  intervalSafe = if interval == 0.0 then 1.0 else interval

  beginning = getBeginning range.min intervalSafe


getBeginning :: Number -> Number -> Number
getBeginning min interval =
  let multiple = min / interval
        -- TODO figure out precision
  in
  if multiple == round multiple then
    min
  else
    ceilingTo interval min


positions :: Range -> Number -> Number -> Number -> Array Number -> Array Number
positions range beginning interval m acc =
  let nextPosition = correctFloat (getPrecision interval) (beginning + (m * interval))
  in
  if nextPosition > range.max then
    acc
  else
    positions range beginning interval (m + 1.0) (acc `snoc` nextPosition)


getInterval :: Number -> Boolean -> Boolean -> Number
getInterval intervalRaw allowDecimals hasTickAmount =
  correctFloat precision (multiple * magnitude)
  where
  magnitude = toMagnitude intervalRaw

  normalized = intervalRaw / magnitude

  multiples = getMultiples magnitude allowDecimals hasTickAmount

  findMultiple multiples_ =
    case multiples_ of
      m1 : m2 : rest ->
        if normalized <= (m1 + m2) / 2.0 then
          m1
        else
          findMultiple (m2 : rest)
      m1 : Nil ->
        if normalized <= m1 then
          m1
        else
          1.0
      _ -> 1.0

  findMultipleExact multiples_ =
    fromMaybe 1.0 $ multiples_ # findMap \m1 ->
      if m1 * magnitude >= intervalRaw then
        Just m1
      else
        Nothing

  multiple =
    if hasTickAmount then
      findMultipleExact multiples
    else
      findMultiple (List.fromFoldable multiples)

  precision =
    getPrecision magnitude + getPrecision multiple


getMultiples :: Number -> Boolean -> Boolean -> Array Number
getMultiples magnitude allowDecimals hasTickAmount =
  if allowDecimals then
    defaults
  else if magnitude == 1.0 then
    filter (\n -> round n == n) defaults
  else if magnitude <= 0.1 then
    [ 1.0 / magnitude ]
  else
    defaults
  
  where
  defaults =
    if hasTickAmount then
      [ 1.0, 1.2, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 8.0, 10.0 ]
    else
      [ 1.0, 2.0, 2.5, 5.0, 10.0 ]


correctFloat :: Int -> Number -> Number
correctFloat prec = F.toStringWith (F.fixed prec) >>> Number.fromString >>> fromMaybe 0.0

getPrecision :: Number -> Int
getPrecision number =
    case split (Pattern "e") (show number) of
        [ _, after ] ->
            maybe 0 abs (Int.fromString after)
        _ ->
            case split (Pattern ".") (show number) of
                [ _, after ] -> String.length after
                _ -> 0


ceilingTo :: Number -> Number -> Number
ceilingTo prec number = prec * (ceil (number / prec))


toMagnitude :: Number -> Number
toMagnitude num = 10.0 `pow` floor (log num / ln10)
-- | SVG path commands.
module Pha.Chart.Internal.Commands(Command(..), description) where

import Prelude
import Data.String (joinWith)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

import Pha.Chart.Internal.Coordinates (Plane, toSVGX, toSVGY)


data Command
  = Move Number Number
  | Line Number Number
  | CubicBeziers Number Number Number Number Number Number
  | CubicBeziersShort Number Number Number Number
  | QuadraticBeziers Number Number Number Number
  | QuadraticBeziersShort Number Number
  | Arc Number Number Int Boolean Boolean Number Number
  | Close


description :: Plane -> Array Command -> String
description plane commands =
  joinCommands $ stringCommand <<< translate plane <$> commands


translate :: Plane -> Command -> Command
translate plane command =
  case command of
    Move x y -> Move (toSVGX plane x) (toSVGY plane y)
    Line x y -> Line (toSVGX plane x) (toSVGY plane y)
    CubicBeziers cx1 cy1 cx2 cy2 x y ->
      CubicBeziers (toSVGX plane cx1) (toSVGY plane cy1) (toSVGX plane cx2) (toSVGY plane cy2) (toSVGX plane x) (toSVGY plane y)
    CubicBeziersShort cx1 cy1 x y ->
      CubicBeziersShort (toSVGX plane cx1) (toSVGY plane cy1) (toSVGX plane x) (toSVGY plane y)
    QuadraticBeziers cx1 cy1 x y ->
      QuadraticBeziers (toSVGX plane cx1) (toSVGY plane cy1) (toSVGX plane x) (toSVGY plane y)
    QuadraticBeziersShort x y ->
      QuadraticBeziersShort (toSVGX plane x) (toSVGY plane y)
    Arc rx ry xAxisRotation largeArcFlag sweepFlag x y ->
      Arc rx ry xAxisRotation largeArcFlag sweepFlag (toSVGX plane x) (toSVGY plane y)
    Close -> Close


stringCommand :: Command -> String
stringCommand command =
  case command of
    Move x y -> "M" <> stringPoint (x /\ y)
    Line x y -> "L" <> stringPoint (x /\ y)
    CubicBeziers cx1 cy1 cx2 cy2 x y -> "C" <> stringPoints [ (cx1 /\ cy1), (cx2 /\ cy2), (x /\ y) ]
    CubicBeziersShort cx1 cy1 x y -> "Q" <> stringPoints [ (cx1 /\ cy1), (x /\ y) ]
    QuadraticBeziers cx1 cy1 x y -> "Q" <> stringPoints [ (cx1 /\ cy1), (x /\ y) ]
    QuadraticBeziersShort x y -> "T" <> stringPoint (x /\ y)
    Arc rx ry xAxisRotation largeArcFlag sweepFlag x y ->
      "A " <> joinCommands
        [ stringPoint (rx /\ ry)
        , show xAxisRotation
        , stringBoolInt largeArcFlag
        , stringBoolInt sweepFlag
        , stringPoint (x /\ y)
        ]
    Close -> "Z"


joinCommands :: Array String -> String
joinCommands = joinWith " "

stringPoint :: Tuple Number Number -> String
stringPoint (x /\ y) = show x <> " " <> show y

stringPoints :: Array (Tuple Number Number ) -> String
stringPoints points = joinWith "," (stringPoint <$> points)


stringBoolInt :: Boolean -> String
stringBoolInt bool = if bool then "1" else "0"
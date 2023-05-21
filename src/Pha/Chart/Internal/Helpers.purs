module Pha.Chart.Internal.Helpers where

import Prelude
import Data.Array ((!!), foldl, groupBy, length, mapWithIndex, tail, zip)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple (Tuple(..))

type Attribute c = c -> c

applyFuncs :: forall a. Array (a -> a) -> a -> a
applyFuncs funcs default = foldl (#) default funcs


withSurround :: forall a b. (Int -> Maybe a -> a -> Maybe a -> b) -> Array a -> Array b
withSurround func xs = xs # mapWithIndex \i x -> func i (xs !! (i - 1)) x (xs !! (i + 1))

--gatherWith :: forall a. (a -> a -> Boolean) -> Array a -> Array (NEA.NonEmptyArray a)
--gatherWith testFn = groupBy testFn

pairwise ∷ ∀ a. Array a → Array (Tuple a a)
pairwise list = maybe [] (zip list) (tail list)


-- DEFAULTS


toDefaultColor :: Int -> String
toDefaultColor = toDefault pink [ purple, pink, blue, green, red, yellow, turquoise, orange, moss, brown ]

toDefault :: forall a. a -> Array a -> Int -> a
toDefault default items index = fromMaybe default $ items !! (index `mod` len)
  where
  len = length items

-- COLORS

pink :: String
pink = "#ea60df"

purple :: String
purple = "#7b4dff"

blue :: String
blue = "#12A5ED"

moss :: String
moss = "#92b42c"

green :: String
green = "#71c614"

orange :: String
orange = "#FF8400"

turquoise :: String
turquoise = "#22d2ba"

red :: String
red = "#F5325B"

darkYellow :: String
darkYellow = "#eabd39"

darkBlue :: String
darkBlue = "#7345f6"

coral :: String
coral = "#ea7369"

magenta :: String
magenta = "#db4cb2"

brown :: String
brown = "#871c1c"

mint :: String
mint = "#6df0d2"

yellow :: String
yellow = "#FFCA00"

gray :: String
gray = "#EFF2FA"

darkGray :: String
darkGray = "rgb(200 200 200)"

labelGray :: String
labelGray = "#808BAB"
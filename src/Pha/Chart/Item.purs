module Pha.Chart.Item
  ( Item
  , Many
  , andThen
  , any
  , apply
  , bars
  , bins
  , dots
  , filter
  , getBottom
  , getBottomLeft
  , getBottomRight
  , getCenter
  , getColor
  , getData
  , getDatas
  , getLeft
  , getLimits
  , getMember
  , getMembers
  , getName
  , getOneData
  , getPosition
  , getRight
  , getSize
  , getTooltip
  , getTooltipValue
  , getTop
  , getTopLeft
  , getTopRight
  , getX
  , getX1
  , getX2
  , getY
  , isReal
  , isSame
  , named
  , real
  , sameX
  , stacks
  , module Exports
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Pha.Html (Html)
import Pha.Chart.Internal.Coordinates (Point, Position, Plane)
import Pha.Chart.Internal.Coordinates as C
import Pha.Chart.Internal.Item as I
import Pha.Chart.Internal.Item (One, Any)
import Pha.Chart.Internal.Many as M
import Pha.Chart.Internal.Many (Remodel)
import Pha.Chart.Internal.Svg as CS
import Pha.Chart.Internal.Svg (Bar, Dot)

import Pha.Chart.Internal.Item (One, Any) as Exports

type Item x = I.Rendered x

-- | Get the default tooltip.
getTooltip :: forall x. Item x -> Array (Html Void)
getTooltip = I.toHtml

getCenter :: forall x. Plane -> Item x -> Point
getCenter p = I.getPosition p >>> C.center

getLeft :: forall x. Plane -> Item x -> Point
getLeft p = I.getPosition p >>> C.left

getRight :: forall x. Plane -> Item x -> Point
getRight p = I.getPosition p >>> C.right

getTop :: forall x. Plane -> Item x -> Point
getTop p = I.getPosition p >>> C.top

getTopLeft :: forall x. Plane -> Item x -> Point
getTopLeft p = I.getPosition p >>> C.topLeft

getTopRight :: forall x. Plane -> Item x -> Point
getTopRight p = I.getPosition p >>> C.topRight

getBottom :: forall x. Plane -> Item x -> Point
getBottom p = I.getPosition p >>> C.bottom

getBottomLeft :: forall x. Plane -> Item x -> Point
getBottomLeft p = I.getPosition p >>> C.bottomLeft

getBottomRight :: forall x. Plane -> Item x -> Point
getBottomRight p = I.getPosition p >>> C.bottomRight

getPosition :: forall x. Plane -> Item x -> Position
getPosition = I.getPosition

-- | In a few cases, a rendered item's "position" and "limits" aren't the same.

-- | In the case of a bin, the "position" is the area which the bins bars take up, not
-- | inclusing any margin which may be around them. Its "limits" include the margin.
getLimits :: forall x. Item x -> Position
getLimits = I.getLimits



-- ONE


-- | Get the data the item was produced from. -}
getData :: forall data_ x. One data_ x -> data_
getData = I.getDatum

-- | Get the x value of the item.
getX :: forall data_ x. One data_ x -> Number
getX = I.getX

-- | Get the x1 value of the item.
getX1 :: forall data_ x. One data_ x -> Number
getX1 = I.getX1

-- | Get the x2 value of the item.
getX2 :: forall data_ x. One data_ x -> Number
getX2 = I.getX2


-- | Get the y value of the item.
getY :: forall data_ x. One data_ x -> Number
getY = I.getY


-- | Get the name of the series which produced the item.
getName :: forall data_ x. One data_ x -> String
getName = I.getName


-- | Get the color of the item.
getColor :: forall data_ x. One data_ x -> String
getColor = I.getColor


-- | Get the formatted y value.
getTooltipValue :: forall data_ x. One data_ x -> String
getTooltipValue = I.getTooltipValue


-- | Get the size of a dot.
getSize :: forall data_. One data_ Dot -> Number
getSize = I.getSize


-- | Is the item a representation of missing data? This may be
-- | the case if you used e.g. `C.scatterMaybe` or `C.barMaybe`.
isReal :: forall data_ x. One data_ x -> Boolean
isReal = I.isReal

-- | Is this item the exact same as the other?
isSame :: forall data_ x. One data_ x -> One data_ x -> Boolean
isSame = I.isSame

-- | Filter for a certain data type.
filter :: forall a b x. (a -> Maybe b) -> Array (One a x) -> Array (One b x)
filter = I.mapMaybeOne

-- MANY

-- | A collection of many items.
-- |
-- | ```purescript
-- | Many data Bar -- representation of several bars
-- | Many data Dot -- representation of several dots
-- | Many data Any -- representation of several dos or bars
-- | ```
-- | Sometimes it's neccessary to work with a group of items, rather
-- | than a single. For example, if you'd like a tooltip to show up
-- | on top of a stacked bar, it's helpful to be able to treat all
-- | the pieces of that stack at the same time.
type Many data_ x = M.Many (One data_ x)

-- | Get all members of the group.
getMembers :: forall data_ x. Many data_ x -> Array (One data_ x)
getMembers = M.getMembers


-- | Get the first members of the group.
getMember :: forall data_ x. Many data_ x -> One data_ x
getMember = M.getMember

-- | Get the data from each member in the group.
getDatas :: forall data_ x. Many data_ x -> Array data_
getDatas = M.getDatas

-- | Get the data from the first member in the group.
getOneData :: forall data_ x. Many data_ x -> data_
getOneData = M.getData



-- REMODELLING

-- | Apply a remodelling.
apply :: forall a b. Remodel a b -> Array a -> Array b
apply = M.apply

-- | Chain a remodelling.
andThen :: forall a b c. Remodel b c -> Remodel a b -> Remodel a c
andThen = M.andThen

-- | Keep anything.
any :: forall data_. Remodel (One data_ Any) (One data_ Any)
any = M.any


-- | Keep only dots.
dots :: forall data_. Remodel (One data_ Any) (One data_ Dot)
dots = M.dots

-- | Keep only bars.
bars :: forall data_. Remodel (One data_ Any) (One data_ Bar)
bars = M.bars

-- | Remove representations of missing data.
real :: forall data_ x. Remodel (One data_ x) (One data_ x)
real = M.real


-- | Keep only items coming from series with the names listed.
named :: forall data_ x. Array String -> Remodel (One data_ x) (One data_ x)
named = M.named


-- | Group into bins. Items are in the same bin
-- | if they are produced from the same element and the
-- | same data point.
bins :: forall data_ x. Remodel (One data_ x) (Many data_ x)
bins = M.bins


-- | Group into bins. Items are in the same stack
-- | if they are produced from the same `C.stacked` property.
stacks :: forall data_ x. Remodel (One data_ x) (Many data_ x)
stacks = M.stacks


-- | Group into items with the same x value.
sameX :: forall data_ x. Remodel (One data_ x) (Many data_ x)
sameX = M.sameX

--customs : Remodel (Any data) (Custom data)


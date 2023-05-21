module Pha.Charts.Internal.Many where

import Prelude
import Data.Array (elem, filter, groupBy, mapMaybe)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Pha.Html as H
import Pha.Charts.Internal.Coordinates (Point, Position, Plane)
import Pha.Charts.Internal.Coordinates as Coord
import Pha.Charts.Internal.Svg as S
import Pha.Charts.Internal.Item as I

type Many x = I.Rendered { items :: NonEmptyArray x }

getMembers :: forall x. Many x -> Array x
getMembers (I.Rendered group_) = NEA.toArray $ group_.config.items

getMember :: forall x. Many x -> x
getMember (I.Rendered group_) = NEA.head $ group_.config.items

getGenerals :: forall d x. Many (I.One d x) -> Array (I.One d I.Any)
getGenerals group = generalize <$> getMembers group
  where
  generalize (I.Rendered item) = I.generalize item.config.toAny (I.Rendered item)

getDatas :: forall d x. Many (I.One d x) -> Array d
getDatas (I.Rendered group) = NEA.toArray $ I.getDatum <$> group.config.items


getData :: forall d x. Many (I.One d x) -> d
getData (I.Rendered group) = I.getDatum $ NEA.head $ group.config.items


mapData :: forall a b x. (a -> b) -> Many (I.One a x) -> Many (I.One b x)
mapData func (I.Rendered group) =
  toGroup $ map (I.mapOne func) group.config.items


-- GROUPING


data Remodel a b =
  Remodel
    (Plane -> b -> Position)
    (Array a -> Array b)

apply :: forall a b. Remodel a b -> Array a -> Array b
apply (Remodel _ func) items =
  func items


andThen :: forall a  x y. Remodel x y -> Remodel a x -> Remodel a y
andThen (Remodel toPos2 func2) (Remodel _ func1) =
  Remodel toPos2 $ func2 <<< func1



-- BASIC GROUPING


any :: forall d. Remodel (I.One d I.Any) (I.One d I.Any)
any = Remodel I.getPosition identity


dots :: forall d. Remodel (I.One d I.Any) (I.One d S.Dot)
dots = Remodel centerPosition (mapMaybe I.isDot)
  where
  centerPosition plane item = fromPoint $ Coord.center $ I.getPosition plane item


bars :: forall d. Remodel (I.One d I.Any) (I.One d S.Bar)
bars = Remodel I.getPosition (mapMaybe I.isBar)


real :: forall d config. Remodel (I.One d config) (I.One d config)
real = Remodel I.getPosition (filter I.isReal)


named :: forall d config. Array String -> Remodel (I.One d config) (I.One d config)
named names = Remodel I.getPosition (filter onlyAcceptedNames)
  where
  onlyAcceptedNames i = elem (I.getName i) names


-- SAME X


sameX :: forall d x. Remodel (I.One d x) (Many (I.One d x))
sameX =
  let fullVertialPosition plane item =
        I.getPosition plane item
          # _ { y1 = plane.y.min, y2 = plane.y.max }
  in
  Remodel fullVertialPosition $
    groupingHelp
      { shared: \config -> { x1: config.values.x1, x2: config.values.x2 }
      , equality: \a b -> a.x1 == b.x1 && a.x2 == b.x2
      , edits: identity
      }



-- SAME STACK


stacks :: forall d x. Remodel (I.One d x) (Many (I.One d x))
stacks =
  Remodel I.getPosition $
    groupingHelp
      { shared: \config ->
            { x1: config.values.x1
            , x2: config.values.x2
            , property: config.tooltipInfo.property
            }
      , equality: \a b -> a.x1 == b.x1 && a.x2 == b.x2 && a.property == b.property
      , edits: identity
      }



-- SAME BIN


bins :: forall d x. Remodel (I.One d x) (Many (I.One d x))
bins =
  Remodel I.getPosition $
    groupingHelp
      { shared: \config ->
          { x1: config.values.x1
          , x2: config.values.x2
          , elIndex: config.tooltipInfo.elIndex
          , dataIndex: config.tooltipInfo.data
          }
      , equality: \a b -> a.x1 == b.x1 && a.x2 == b.x2 && a.elIndex == b.elIndex && a.dataIndex == b.dataIndex
      , edits: editLimits \item pos -> pos {x1 = I.getX1 item, x2 = I.getX2 item}
      }



-- HELPERS


groupingHelp :: forall a x.
  { shared :: x -> a
  , equality :: a -> a -> Boolean
  , edits :: Many (I.Rendered x) -> Many (I.Rendered x)
  }
  -> Array (I.Rendered x)
  -> Array (Many (I.Rendered x))
groupingHelp { shared, equality, edits } items =
  toNewGroup <$> groupBy toEquality items
  where
  toShared (I.Rendered item) = shared item.config
  toEquality aO bO = equality (toShared aO) (toShared bO)
  toNewGroup is = edits $ toGroup is


editLimits :: forall x. (x -> Position -> Position) -> Many x -> Many x
editLimits edit (I.Rendered group) =
  I.Rendered $ group { toLimits = \c -> c.items # \xs -> group.toLimits c # edit (NEA.head xs) }


toGroup :: forall x. NonEmptyArray (I.Rendered x) -> Many (I.Rendered x)
toGroup xs =
  I.Rendered
    { config: { items: xs }
    , toLimits: \c -> Coord.foldPosition I.getLimits (NEA.toArray c.items)
    , toPosition: \p c -> Coord.foldPosition (I.getPosition p) (NEA.toArray c.items)
    , toSvg: \p c _ -> H.g [ H.class_ "elm-charts__group" ] (I.toSvg p <$> NEA.toArray c.items)
    , toHtml: \c -> [ H.elem "table" [] (NEA.toArray c.items >>= I.toHtml) ]
    }


fromPoint :: Point -> Position
fromPoint point = { x1: point.x, y1: point.y, x2: point.x, y2: point.y }
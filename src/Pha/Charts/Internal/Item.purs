module Pha.Charts.Internal.Item where

import Prelude
import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Pha.Html (Html)
import Pha.Html as H
import Pha.Charts.Internal.Coordinates (Plane, Position)
import Pha.Charts.Internal.Svg as S

newtype Rendered a =
  Rendered
    { config :: a
    , toLimits :: a -> Position
    , toPosition :: Plane -> a -> Position
    , toSvg :: Plane -> a -> Position -> Html Void
    , toHtml :: a -> Array (Html Void)
    }


type One d x =
  Rendered
    { product :: x
    , tooltipInfo :: TooltipInfo
    , values :: Values d
    , toAny :: x -> Any
    }


data Any = Dot S.Dot | Bar S.Bar | Custom


type TooltipInfo =
  { property :: Int
  , stack :: Int
  , data :: Int
  , index :: Int
  , elIndex :: Int
  , name :: Maybe String
  , color :: String
  , border :: String
  , borderWidth :: Number
  , formatted :: String
  }

type Values d =
  { datum :: d
  , x1 :: Number
  , x2 :: Number
  , y :: Number
  , isReal :: Boolean
  }


-- ITEM

toSvg :: forall x. Plane -> Rendered x -> Html Void
toSvg plane (Rendered item) =
  item.toSvg plane item.config (item.toPosition plane item.config)

toHtml :: forall x. Rendered x -> Array (Html Void)
toHtml (Rendered item) = item.toHtml item.config

getPosition :: forall x. Plane -> Rendered x -> Position
getPosition plane (Rendered item) = item.toPosition plane item.config

getLimits :: forall x. Rendered x -> Position
getLimits (Rendered item) = item.toLimits item.config

getColor :: forall d x. One d x -> String
getColor (Rendered item) = item.config.tooltipInfo.color

getName :: forall d x. One d x -> String
getName (Rendered item) =
  case item.config.tooltipInfo.name of
    Just name -> name
    Nothing -> "Property #" <> show (item.config.tooltipInfo.index + 1)


getDatum :: forall d x. One d x -> d
getDatum (Rendered item) = item.config.values.datum

getX :: forall d x. One d x -> Number
getX (Rendered item) = item.config.values.x1

getX1 :: forall d x. One d x -> Number
getX1 (Rendered item) = item.config.values.x1

getX2 :: forall d x. One d x -> Number
getX2 (Rendered item) = item.config.values.x2

getY :: forall d x. One d x -> Number
getY (Rendered item) = item.config.values.y


isReal :: forall d x. One d x -> Boolean
isReal (Rendered item) = item.config.values.isReal

getElIndex :: forall d x. One d x -> Int
getElIndex (Rendered item) = item.config.tooltipInfo.elIndex

getPropertyIndex :: forall d x. One d x -> Int
getPropertyIndex (Rendered item) = item.config.tooltipInfo.property

getStackIndex :: forall d x. One d x -> Int
getStackIndex (Rendered item) = item.config.tooltipInfo.stack

getDataIndex :: forall d x. One d x -> Int
getDataIndex (Rendered item) = item.config.tooltipInfo.data

getTooltipValue :: forall d x. One d x -> String
getTooltipValue (Rendered item) = item.config.tooltipInfo.formatted

getGeneral :: forall d x. One d x -> One d Any
getGeneral (Rendered item) =
  generalize item.config.toAny (Rendered item)

getSize :: forall d. One d S.Dot -> Number
getSize (Rendered item) = item.config.product.size


mapOne :: forall a b x. (a -> b) -> One a x -> One b x
mapOne func (Rendered item) =
  Rendered
    { toLimits: \_ -> item.toLimits item.config
    , toPosition: \plane _ -> item.toPosition plane item.config
    , toSvg: \plane _ _ -> toSvg plane (Rendered item)
    , toHtml: \_ -> toHtml (Rendered item)
    , config:
        { product: item.config.product
        , values:
            { datum: func item.config.values.datum
            , x1: item.config.values.x1
            , x2: item.config.values.x2
            , y: item.config.values.y
            , isReal: item.config.values.isReal
            }
        , tooltipInfo: item.config.tooltipInfo
        , toAny: item.config.toAny
        }
    }

mapMaybeOne :: forall a b x. (a -> Maybe b) -> Array (One a x) -> Array (One b x)
mapMaybeOne func =
  mapMaybe $ \(Rendered item) ->
    case func item.config.values.datum of
      Just b ->
        Just $ Rendered
          { toLimits: \_ -> item.toLimits item.config
          , toPosition: \plane _ -> item.toPosition plane item.config
          , toSvg: \plane _ _ -> toSvg plane (Rendered item)
          , toHtml: \_ -> toHtml (Rendered item)
          , config:
              { product: item.config.product
              , values:
                  { datum: b
                  , x1: item.config.values.x1
                  , x2: item.config.values.x2
                  , y: item.config.values.y
                  , isReal: item.config.values.isReal
                  }
              , tooltipInfo: item.config.tooltipInfo
              , toAny: item.config.toAny
              }
          }

      Nothing ->
        Nothing


-- GENERALIZATION

generalize :: forall d x. (x -> Any) -> One d x -> One d Any
generalize toAny (Rendered item) =
   -- TODO make sure changes are reflected in rendering
  Rendered
    { toLimits: \_ -> item.toLimits item.config
    , toPosition: \plane _ -> item.toPosition plane item.config
    , toSvg: \plane _ _ -> toSvg plane (Rendered item)
    , toHtml: \_ -> toHtml (Rendered item)
    , config:
        { product: toAny item.config.product
        , values: item.config.values
        , tooltipInfo: item.config.tooltipInfo
        , toAny: identity
        }
    }

isBar :: forall d. One d Any -> Maybe (One d S.Bar)
isBar (Rendered item) =
  case item.config.product of
    Bar bar ->
      Just $ Rendered
        { toLimits: \_ -> item.toLimits item.config
        , toPosition: \plane _ -> item.toPosition plane item.config
        , toSvg: \plane config -> S.bar plane config.product
        , toHtml: \_ -> item.toHtml item.config
        , config:
            { product: bar
            , values: item.config.values
            , tooltipInfo: item.config.tooltipInfo
            , toAny: Bar
            }
        }

    _ ->
      Nothing


isDot :: forall d. One d Any -> Maybe (One d S.Dot)
isDot (Rendered item) =
  case item.config.product of
    Dot dot ->
      Just $ Rendered
        { toLimits: \_ -> item.toLimits item.config
        , toPosition: \plane _ -> item.toPosition plane item.config
        , toSvg: \plane config _ ->
            if config.values.isReal
            then S.dot plane _.x _.y config.product { x: config.values.x1, y: config.values.y }
            else H.empty
        , toHtml: \_ -> item.toHtml item.config
        , config:
            { product: dot
            , values: item.config.values
            , tooltipInfo: item.config.tooltipInfo
            , toAny: Dot
            }
        }
    _ ->
      Nothing
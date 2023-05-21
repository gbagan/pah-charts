module Pha.Chart.Internal.Produce where

import Prelude
import Data.Array (catMaybes, concat, head, length, mapWithIndex, reverse)
import Data.Array.NonEmpty as NEA
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Chart.Attributes as CA
import Pha.Chart.Internal.Coordinates as Coord
import Pha.Chart.Internal.Property as IP
import Pha.Chart.Internal.Item as I
import Pha.Chart.Internal.Many as M
import Pha.Chart.Internal.Svg as S
import Pha.Chart.Internal.Helpers (applyFuncs, withSurround, toDefaultColor)

type Bars d =
  { spacing :: Number
  , margin :: Number
  , roundTop :: Number
  , roundBottom :: Number
  , grouped :: Boolean
  , grid :: Boolean
  , x1 :: Maybe (d -> Number)
  , x2 :: Maybe (d -> Number)
  }

defaultBars :: forall d. Bars d
defaultBars =
  { spacing: 0.05
  , margin: 0.1
  , roundTop: 0.0
  , roundBottom: 0.0
  , grouped: true
  , grid: false
  , x1: Nothing
  , x2: Nothing
  }

toBarSeries :: forall d
                . Int
                -> Array (CA.Attribute (Bars d)) 
                -> Array (IP.Property d String Unit S.Bar) 
                -> Array d 
                -> Array (M.Many (I.One d S.Bar))
toBarSeries elIndex barsAttrs properties data_ =
  withSurround toBin data_ # \bins ->
    map IP.toConfigs properties
      # mapWithIndex (\barIndex stacks -> mapWithIndex (toSeriesItem bins stacks barIndex) (reverse stacks))
      # concat
      # mapWithIndex (\propIndex f -> f (elIndex + propIndex))
      # catMaybes
  where
  barsConfig = applyFuncs barsAttrs defaultBars

  toBarConfig attrs = applyFuncs attrs S.defaultBar

  toBin index prevM curr nextM =
        case barsConfig.x1, barsConfig.x2 of
          Nothing, Nothing ->
            { datum: curr, start: toNumber (index + 1) - 0.5, end: toNumber (index + 1) + 0.5 }
          Just toStart, Nothing ->
            case prevM, nextM of
              _, Just next ->
                { datum: curr, start: toStart curr, end: toStart next }
              Just prev, Nothing ->
                { datum: curr, start: toStart curr, end: toStart curr + (toStart curr - toStart prev) }
              Nothing, Nothing ->
                { datum: curr, start: toStart curr, end: toStart curr + 1.0 }
          Nothing, Just toEnd ->
            case prevM, nextM of
              Just prev, _  ->
                { datum: curr, start: toEnd prev, end: toEnd curr }
              Nothing, Just next ->
                { datum: curr, start: toEnd curr - (toEnd next - toEnd curr), end: toEnd curr }
              Nothing, Nothing ->
                { datum: curr, start: toEnd curr - 1.0, end: toEnd curr }

          Just toStart, Just toEnd ->
            { datum: curr, start: toStart curr, end: toEnd curr }


  toSeriesItem bins sections barIndex sectionIndex section colorIndex = do
    xs <- NEA.fromArray $ mapWithIndex (toBarItem sections barIndex sectionIndex section colorIndex) bins
    Just $ I.Rendered
      { config: { items: xs }
      , toLimits: \c -> Coord.foldPosition I.getLimits (NEA.toArray c.items)
      , toPosition: \plane c -> Coord.foldPosition (I.getPosition plane) (NEA.toArray c.items)
      , toSvg: \plane c _ -> H.g [ H.class_ "elm-charts__bar-series" ] $
                  I.toSvg plane <$> (NEA.toArray c.items)
      , toHtml: \c -> [ H.table [ H.style "margin" "0" ] (NEA.toArray c.items >>= I.toHtml) ]
      }

  toBarItem sections barIndex sectionIndex section colorIndex dataIndex bin =
    I.Rendered
          { config:
              { product: product
              , values:
                  { datum: bin.datum
                  , x1: start
                  , x2: end
                  , y: fromMaybe 0.0 value
                  , isReal: isJust value
                  }
              , tooltipInfo:
                  { property: barIndex
                  , stack: sectionIndex
                  , data: dataIndex
                  , index: colorIndex
                  , elIndex: elIndex
                  , name: section.meta
                  , color: product.color
                  , border: product.border
                  , borderWidth: product.borderWidth
                  , formatted: section.format bin.datum
                  }
              , toAny: I.Bar
              }
          , toLimits: \_ -> { x1, x2, y1: min y1 y2, y2: max y1 y2 }
          , toPosition: \_ _ -> { x1, x2, y1, y2 }
          , toSvg: \plane _ position -> S.bar plane product position
          , toHtml: \c -> [ tooltipRow c.tooltipInfo.color (toDefaultName colorIndex c.tooltipInfo.name) (section.format bin.datum) ]
          }
    where
    numOfBars = if barsConfig.grouped then toNumber (length properties) else 1.0
    numOfSections = toNumber (length sections)

    start = bin.start
    end = bin.end
    visual = section.visual bin.datum
    value = section.value bin.datum

    length_ = end - start
    margin = length_ * barsConfig.margin
    spacing = length_ * barsConfig.spacing
    width = (length_ - margin * 2.0 - (numOfBars - 1.0) * spacing) / numOfBars
    offset = if barsConfig.grouped then toNumber barIndex * width + toNumber barIndex * spacing else 0.0

    x1 = start + margin + offset
    x2 = start + margin + offset + width
    minY = if numOfSections > 1.0 then max 0.0 else identity
    y1 = minY $ fromMaybe 0.0 visual - fromMaybe 0.0 value
    y2 = minY $ fromMaybe 0.0 visual

    isFirst = sectionIndex == 0
    isLast = toNumber sectionIndex == numOfSections - 1.0
    isSingle = numOfSections == 1.0

    roundTop = if isSingle || isLast then barsConfig.roundTop else 0.0
    roundBottom = if isSingle || isFirst then barsConfig.roundBottom else 0.0

    defaultColor = toDefaultColor colorIndex
    defaultAttrs = [ CA.roundTop roundTop, CA.roundBottom roundBottom, CA.color defaultColor, CA.border defaultColor ]
    attrs = defaultAttrs <> section.attrs <> section.extra barIndex sectionIndex dataIndex section.meta bin.datum
    productOrg = toBarConfig attrs
    product =
      productOrg
        # (\p ->
            case p.design of
              Just (S.Gradient clrs) ->
                case head clrs of
                  Just color -> if p.color == defaultColor then p { color = color } else p
                  Nothing -> p
              _ -> p)
        # (\p -> if p.border == defaultColor then p { border = p.color } else p)


{-| -}
toDotSeries :: forall data_
              . Int 
              -> (data_ -> Number) 
              -> Array (IP.Property data_ String S.Interpolation S.Dot) 
              -> Array data_ 
              -> Array (M.Many (I.One data_ S.Dot))
toDotSeries elIndex toX properties data_ =
  map IP.toConfigs properties
    # mapWithIndex (\lineIndex stacks -> mapWithIndex (toSeriesItem lineIndex stacks) stacks)
    # concat
    # mapWithIndex (\propIndex f -> f (elIndex + propIndex))
    # catMaybes

  where
  toInterConfig attrs = applyFuncs attrs S.defaultInterpolation
  
  toDotConfig attrs = applyFuncs attrs S.defaultDot

  toSeriesItem lineIndex stacks stackIndex prop colorIndex =
        let dotItems = mapWithIndex (toDotItem lineIndex stackIndex colorIndex prop interConfig) data_
            defaultOpacity = if length stacks > 1 then 0.4 else 0.0
            interAttr = [ CA.color (toDefaultColor colorIndex), CA.opacity defaultOpacity ] <> prop.inter
            interConfig = toInterConfig interAttr
        in do
        xs <- NEA.fromArray dotItems
        Just $ I.Rendered
              { config: { items: xs }
              , toSvg: \plane _ _ ->
                  let toBottom datum_ =
                        (\real visual -> visual - real) <$> prop.value datum_ <*> prop.visual datum_
                  in
                  H.g
                    [ H.class_ "elm-charts__series" ]
                    [ S.area plane toX (Just toBottom) prop.visual interConfig data_
                    , S.interpolation plane toX prop.visual interConfig data_
                    , H.g [ H.class_ "elm-charts__dots" ] (map (I.toSvg plane) dotItems)
                    ]
              , toLimits: \c -> Coord.foldPosition I.getLimits (NEA.toArray c.items)
              , toPosition: \plane c -> Coord.foldPosition (I.getPosition plane) (NEA.toArray c.items)
              , toHtml: \c -> [ H.table [ H.style "margin" "0" ] (NEA.toArray c.items >>= I.toHtml) ]
              }

  toDotItem lineIndex stackIndex colorIndex prop interConfig dataIndex datum_ =
        let defaultAttrs = 
              [ CA.color interConfig.color
              , CA.border interConfig.color
              , if isNothing interConfig.method then CA.circle else identity
              ]
            dotAttrs = defaultAttrs <> prop.attrs <> prop.extra lineIndex stackIndex dataIndex prop.meta datum_
            config = toDotConfig dotAttrs
            x_ = toX datum_
            y_ = fromMaybe 0.0 (prop.visual datum_)
        in
        I.Rendered
          { toSvg: \plane _ _ ->
              case prop.value datum_ of
                Nothing -> H.empty
                Just _ -> S.dot plane _.x _.y config { x: x_, y: y_ }
          , toHtml: \c -> [ tooltipRow c.tooltipInfo.color (toDefaultName colorIndex c.tooltipInfo.name) (prop.format datum_) ]
          , toLimits: \_ -> { x1: x_, x2: x_, y1: y_, y2: y_ }
          , toPosition: \plane _ ->
              let radius = config.shape # maybe 0.0 (S.toRadius config.size)
                  radiusX_ = Coord.scaleCartesianX plane radius
                  radiusY_ = Coord.scaleCartesianY plane radius
              in
              { x1: x_ - radiusX_
              , x2: x_ + radiusX_
              , y1: y_ - radiusY_
              , y2: y_ + radiusY_
              }
          , config:
              { product: config
              , values:
                  { datum: datum_
                  , x1: x_
                  , x2: x_
                  , y: y_
                  , isReal: isJust (prop.value datum_)
                  }
              , tooltipInfo:
                  { property: lineIndex
                  , stack: stackIndex
                  , data: dataIndex
                  , index: colorIndex
                  , elIndex: elIndex
                  , name: prop.meta
                  , color:
                      case config.color of
                        "white" -> interConfig.color
                        _ -> config.color
                  , border: config.border
                  , borderWidth: config.borderWidth
                  , formatted: prop.format datum_
                  }
              , toAny: I.Dot
              }
          }

-- RENDER


tooltipRow :: forall msg. String -> String -> String -> Html msg
tooltipRow color title text =
  H.tr
    []
    [ H.td
        [ H.style "color" color
        , H.style "padding" "0"
        , H.style "padding-right" "3px"
        ]
        [ H.text (title <> ":") ]
    , H.td
        [ H.style "text-align" "right"
        , H.style "padding" "0"
        ]
        [ H.text text ]
    ]


toDefaultName :: Int -> Maybe String -> String
toDefaultName index name = fromMaybe ("Property #" <> show (index + 1)) name
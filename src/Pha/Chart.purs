module Pha.Chart where

import Prelude
import Data.Array (any, elem, foldl, length, mapMaybe, snoc)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested ((/\))
import Pha.Html as H
import Pha.Html (Html)
import Pha.Chart.Internal.Coordinates as C
import Pha.Chart.Internal.Property as IP
import Pha.Chart.Internal.Item as Item
import Pha.Chart.Internal.Produce as Produce
import Pha.Chart.Internal.Legend as Legend
import Pha.Chart.Internal.Many as Many
import Pha.Chart.Internal.Helpers as Helpers
import Pha.Chart.Internal.Svg as IS
import Pha.Chart.Internal.Svg (Tick)
-- import Internal.Events as IE
import Pha.Chart.Svg as CS
import Pha.Chart.Attributes as CA
import Pha.Chart.Attributes (Attribute)
-- import Pha.Chart.Events as CE
import Pha.Chart.Item as CI

type Container :: Type -> Type -> Type
type Container data_ msg =
  { width :: Number
  , height :: Number
  , margin :: { top :: Number, bottom :: Number, left :: Number, right :: Number }
  , padding :: { top :: Number, bottom :: Number, left :: Number, right :: Number }
  , responsive :: Boolean
  , range :: Array (Attribute C.Axis)
  , domain :: Array (Attribute C.Axis)
  -- , events :: Array (CE.Event data_ msg)
  , htmlAttrs :: Array (H.Prop msg)
  , attrs :: Array (H.Prop msg)
  }

chart :: forall data_ msg. Array (Attribute (Container data_ msg)) -> Array (Element data_ msg) -> Html msg
chart edits unindexedElements =
  IS.container plane
    { attrs: config.attrs
    , htmlAttrs: config.htmlAttrs
    , responsive: config.responsive
    , events: [] -- map toEvent config.events TODO
    }
    before
    chart_
    after
  where
  config =
    Helpers.applyFuncs edits
      { width: 300.0
      , height: 300.0
      , margin: { top: 0.0, bottom: 0.0, left: 0.0, right: 0.0 }
      , padding: { top: 0.0, bottom: 0.0, left: 0.0, right: 0.0 }
      , responsive: true
      , range: []
      , domain: []
      -- , events: [] todo
      , attrs: [ H.style "overflow" "visible" ]
      , htmlAttrs: []
      }

  indexedElements =
    fst $ foldl toIndexedEl ([] /\ 0 ) unindexedElements
    where
    toIndexedEl (acc /\ index) el =
      case el of
        Indexed toElAndIndex ->
          let newEl /\ newIndex = toElAndIndex index
          in
          (acc <> [newEl]) /\ newIndex
        ArrayOfElements els ->
          foldl toIndexedEl (acc /\ index) els
        _ -> 
          (acc <> [el]) /\ index

  elements = if any isGrid indexedElements then indexedElements else [grid []] <> indexedElements
    where
    isGrid el =
      case el of
        GridElement _ -> true
        _ -> false 

  plane = definePlane config elements

  items = getItems plane elements

  legends_ = getLegends elements

  tickValues = getTickValues plane items elements

  {before, chart: chart_, after} =
    viewElements config plane tickValues items legends_ elements

  -- toEvent (IE.Event event_) =
  --      let (IE.Decoder decoder) = event_.decoder in
  --      IS.Event event_.name (decoder items)

data Element data_ msg
  = Indexed (Int -> Tuple (Element data_ msg) Int)
  | SeriesElement
      (Array C.Position)
      (Array (CI.One data_ CI.Any))
      (Array Legend.Legend)
      (C.Plane -> Html msg)
  | BarsElement
      (Array C.Position)
      (Array (CI.One data_ CI.Any))
      (Array Legend.Legend)
      (C.Plane -> TickValues -> TickValues)
      (C.Plane -> Html msg)
  | CustomElement
      (CI.One data_ CI.Any)
      (C.Plane -> Html msg)
  | AxisElement
      (C.Plane -> TickValues -> TickValues)
      (C.Plane -> Html msg)
  | TicksElement
      (C.Plane -> TickValues -> TickValues)
      (C.Plane -> Html msg)
  | TickElement
      (C.Plane -> Tick)
      (C.Plane -> Tick -> TickValues -> TickValues)
      (C.Plane -> Tick -> Html msg)
  | LabelsElement
      (C.Plane -> Labels)
      (C.Plane -> Labels -> TickValues -> TickValues)
      (C.Plane -> Labels -> Html msg)
  | LabelElement
      (C.Plane -> Label)
      (C.Plane -> Label -> TickValues -> TickValues)
      (C.Plane -> Label -> Html msg)
  | GridElement
      (C.Plane -> TickValues -> Html msg)
  | SubElements
      (C.Plane -> Array (CI.One data_ CI.Any) -> Array (Element data_ msg))
  | ArrayOfElements
      (Array (Element data_ msg))
  | SvgElement
      (C.Plane -> Html msg)
  | HtmlElement
      (C.Plane -> Array Legend.Legend -> Html msg)


definePlane :: forall data_ msg. Container data_ msg -> Array (Element data_ msg) -> C.Plane
definePlane config elements =
  { x: 
      calcRange 
      { length = config.width
      , min = min xMin xMax
      , max = max xMin xMax
      }
  , y:
      calcDomain
      { length = config.height
      , min = min yMin yMax
      , max = max yMin yMax
      }
  }
  where
  collectLimits acc el =
        case el of
          Indexed _ -> acc
          SeriesElement lims _ _ _ -> acc <> lims
          BarsElement lims _ _ _ _ -> acc <> lims
          CustomElement _ _ -> acc
          AxisElement _ _ -> acc
          TicksElement _ _ -> acc
          TickElement _ _ _ -> acc
          LabelsElement _ _ _ -> acc
          LabelElement _ _ _ -> acc
          GridElement _ -> acc
          SubElements _ -> acc
          ArrayOfElements subs -> foldl collectLimits acc subs
          SvgElement _ -> acc
          HtmlElement _ -> acc

  width = max 1.0 (config.width - config.padding.left - config.padding.right)
  height = max 1.0 (config.height - config.padding.bottom - config.padding.top)

  limits_ =
        foldl collectLimits [] elements
          # C.foldPosition identity
          # \pos ->
                { x: toLimit width config.margin.left config.margin.right pos.x1 pos.x2
                , y: toLimit height config.margin.top config.margin.bottom pos.y1 pos.y2
                }
          # \{ x, y } -> { x: fixSingles x, y: fixSingles y }

  toLimit length marginMin marginMax min max =
        { length: length
        , marginMin: marginMin
        , marginMax: marginMax
        , min: min
        , max: max
        , dataMin: min
        , dataMax: max
        }

  fixSingles bs =
    if bs.min == bs.max then bs { max = bs.min + 10.0 } else bs

  calcRange =
    case config.range of
      [] -> limits_.x
      some -> foldl (#) limits_.x some

  calcDomain =
    case config.domain of
      [] -> CA.lowest 0.0 CA.orLower limits_.y
      some -> foldl (#) limits_.y some

  unpadded =
    { x: calcRange
    , y: calcDomain
    }

  scalePadX = C.scaleCartesianX unpadded

  scalePadY = C.scaleCartesianY unpadded

  xMin = calcRange.min - scalePadX config.padding.left
  xMax = calcRange.max + scalePadX config.padding.right

  yMin = calcDomain.min - scalePadY config.padding.bottom
  yMax = calcDomain.max + scalePadY config.padding.top


getItems :: forall data_ msg. C.Plane -> Array (Element data_ msg) -> Array (CI.One data_ CI.Any)
getItems _ elements =
  foldl toItems [] elements
  where
  toItems acc el =
        case el of
          Indexed _ -> acc
          SeriesElement _ items _ _ -> acc <> items
          BarsElement _ items _ _ _ -> acc <> items
          CustomElement item _ -> acc <> [ item ]
          AxisElement _ _ -> acc
          TicksElement _ _ -> acc
          TickElement _ _ _ -> acc
          LabelsElement _ _ _ -> acc
          LabelElement _ _ _ -> acc
          GridElement _ -> acc
          SubElements _ -> acc -- TODO add phantom type to only allow decorative els in this
          ArrayOfElements subs -> foldl toItems acc subs
          SvgElement _ -> acc
          HtmlElement _ -> acc

getLegends :: forall data_ msg. Array (Element data_ msg) -> Array Legend.Legend
getLegends elements =
  foldl toLegends [] elements
  where
  toLegends acc el =
        case el of
          Indexed _ -> acc
          SeriesElement _ _ legends_ _ -> acc <> legends_
          BarsElement _ _ legends_ _ _ -> acc <> legends_
          CustomElement _ _ -> acc
          AxisElement _ _ -> acc
          TicksElement _ _ -> acc
          TickElement _ _ _ -> acc
          LabelsElement _ _ _ -> acc
          LabelElement _ _ _ -> acc
          GridElement _ -> acc
          SubElements _ -> acc
          ArrayOfElements subs -> foldl toLegends acc subs
          SvgElement _ -> acc
          HtmlElement _ -> acc
  
type TickValues =
  { xAxis :: Array Number
  , yAxis :: Array Number
  , xs :: Array Number
  , ys :: Array Number
  }

getTickValues :: forall data_ msg. C.Plane -> Array (CI.One data_ CI.Any) -> Array (Element data_ msg) -> TickValues
getTickValues plane items elements =
  foldl toValues {xAxis: [], yAxis: [], xs: [], ys: []} elements
  where
  toValues acc el =
        case el of
          Indexed _ -> acc
          SeriesElement _ _ _ _     -> acc
          BarsElement _ _ _ func _  -> func plane acc
          CustomElement _ _         -> acc
          AxisElement func _        -> func plane acc
          TicksElement func _       -> func plane acc
          TickElement toC func _    -> func plane (toC plane) acc
          LabelsElement toC func _  -> func plane (toC plane) acc
          LabelElement toC func _   -> func plane (toC plane) acc
          SubElements func          -> foldl toValues acc (func plane items)
          GridElement _             -> acc
          ArrayOfElements subs      -> foldl toValues acc subs
          SvgElement _              -> acc
          HtmlElement _             -> acc

viewElements :: forall data_ msg
                . Container data_ msg
                -> C.Plane
                -> TickValues 
                -> Array (CI.One data_ CI.Any) 
                -> Array Legend.Legend 
                -> Array (Element data_ msg) 
                -> { before :: Array (Html msg), chart :: Array (Html msg), after :: Array (Html msg) }
viewElements _ plane tickValues allItems allLegends elements =
  foldl viewOne {before: [], chart: [], after: []} elements
  where
  viewOne {before, chart: chart_, after} el =
        case el of
          Indexed _                 -> { before, chart: chart_, after }
          SeriesElement _ _ _ view  -> { before, chart: chart_ `snoc` view plane, after }
          BarsElement _ _ _ _ view  -> { before, chart: chart_ `snoc` view plane, after }
          CustomElement _ view      -> { before, chart: chart_ `snoc` view plane, after }
          AxisElement _ view        -> { before, chart: chart_ `snoc` view plane, after }
          TicksElement _ view       -> { before, chart: chart_ `snoc` view plane, after }
          TickElement toC _ view    -> { before, chart: chart_ `snoc` view plane (toC plane), after }
          LabelsElement toC _ view  -> { before, chart: chart_ `snoc` view plane (toC plane), after }
          LabelElement toC _ view   -> { before, chart: chart_ `snoc` view plane (toC plane), after }
          GridElement view          -> { before, chart: chart_ `snoc` view plane tickValues, after }
          SubElements func          -> foldl viewOne { before, chart: chart_, after } (func plane allItems)
          ArrayOfElements els       -> foldl viewOne { before, chart: chart_, after } els
          SvgElement view           -> { before, chart: chart_ `snoc` view plane, after }
          HtmlElement view          ->
            { before: if length chart_ > 0 then before `snoc` view plane allLegends else before
            , chart: chart_
            , after: if length chart_ > 0 then after else after `snoc` view plane allLegends
            }
  

type Labels =
  { color :: String
  , pinned :: C.Axis -> Number
  , limits :: Array (Attribute C.Axis)
  , xOff :: Number
  , yOff :: Number
  , flip :: Boolean
  , amount :: Int
  , anchor :: Maybe IS.Anchor
  , generate :: IS.TickType
  , fontSize :: Maybe Int
  , uppercase :: Boolean
  , format :: Maybe (Number -> String)
  , rotate :: Number
  , grid :: Boolean
  , hideOverflow :: Boolean
  , attrs :: Array (H.Prop Void)
  , ellipsis :: Maybe { width :: Number, height :: Number }
  }

xLabels :: forall item msg. Array (Attribute Labels) -> Element item msg
xLabels edits =
  LabelsElement toConfig toTickValues \p config ->
    let default = IS.defaultLabel
        toLabel item =
          IS.label p
            default
            { xOff = config.xOff
            , yOff = if config.flip then -config.yOff + 10.0 else config.yOff
            , color = config.color
            , anchor = config.anchor
            , fontSize = config.fontSize
            , uppercase = config.uppercase
            , rotate = config.rotate
            , attrs = config.attrs
            , hideOverflow = config.hideOverflow
            , ellipsis = config.ellipsis
            }
            [ H.text item.label ]
            { x: item.value
            , y: config.pinned p.y
            }
    in H.g [ H.class_ "elm-charts__x-labels" ] (toLabel <$> toTicks p config)
  where
  toConfig _ =
        Helpers.applyFuncs edits
          { color: "#808BAB"
          , limits: []
          , pinned: CA.zero
          , amount: 5
          , generate: IS.Numbers
          , flip: false
          , anchor: Nothing
          , xOff: 0.0
          , yOff: 18.0
          , grid: false
          , format: Nothing
          , uppercase: false
          , rotate: 0.0
          , fontSize: Nothing
          , attrs: []
          , hideOverflow: false
          , ellipsis: Nothing
          }

  toTicks p config =
    foldl (#) p.x config.limits
        # generateValues config.amount config.generate config.format

  toTickValues p config ts =
    if not config.grid then
      ts
    else
      ts { xs = ts.xs <> map _.value (toTicks p config) }

{-| Produce a set of labels at "nice" numbers on the y-axis of your chart.
The styling options are the same as for `xLabels`.
-}
yLabels :: forall item msg. Array (Attribute Labels) -> Element item msg
yLabels edits =
  LabelsElement toConfig toTickValues \p config ->
    let default = IS.defaultLabel
        toLabel item =
          IS.label p
            default
            { xOff = if config.flip then -config.xOff else config.xOff
            , yOff = config.yOff
            , color = config.color
            , fontSize = config.fontSize
            , uppercase = config.uppercase
            , rotate = config.rotate
            , attrs = config.attrs
            , ellipsis = config.ellipsis
            , hideOverflow = config.hideOverflow
            , anchor =
                case config.anchor of
                  Nothing -> Just (if config.flip then IS.Start else IS.End)
                  Just anchor -> Just anchor
            }
            [ H.text item.label ]
            { x: config.pinned p.x
            , y: item.value
            }
    in
    H.g [ H.class_ "elm-charts__y-labels" ] (map toLabel (toTicks p config))
  where
  toConfig _ =
        Helpers.applyFuncs edits
          { color: "#808BAB"
          , limits: []
          , pinned: CA.zero
          , amount: 5
          , generate: IS.Numbers
          , anchor: Nothing
          , flip: false
          , xOff: -10.0
          , yOff: 3.0
          , grid: false
          , format: Nothing
          , uppercase: false
          , fontSize: Nothing
          , rotate: 0.0
          , attrs: []
          , hideOverflow: false
          , ellipsis: Nothing
          }

  toTicks p config =
    foldl (#) p.y config.limits
    # generateValues config.amount config.generate config.format

  toTickValues p config ts =
    if not config.grid then
      ts
    else
      ts { ys = ts.ys <> map _.value (toTicks p config) }


type Label =
  { x :: Number
  , y :: Number
  , xOff :: Number
  , yOff :: Number
  , border :: String
  , borderWidth :: Number
  , fontSize :: Maybe Int
  , color :: String
  , anchor :: Maybe IS.Anchor
  , rotate :: Number
  , uppercase :: Boolean
  , flip :: Boolean
  , grid :: Boolean
  , hideOverflow :: Boolean
  , attrs :: Array (H.Prop Void)
  , ellipsis :: Maybe { width :: Number, height :: Number }
  }


type Grid =
  { color :: String
  , width :: Number
  , dotGrid :: Boolean
  , dashed :: Array Number
  }



grid :: forall item msg. Array (Attribute Grid) -> Element item msg
grid edits =
  GridElement \p vs ->
    H.g [ H.class_ "elm-charts__grid" ] $
      if config.dotGrid then
        vs.xs >>= \x -> mapMaybe (toDot vs p x) vs.ys
      else
        [ H.g [ H.class_ "elm-charts__x-grid" ] (mapMaybe (toXGrid vs p) vs.xs)
        , H.g [ H.class_ "elm-charts__y-grid" ] (mapMaybe (toYGrid vs p) vs.ys)
        ]
  where
  config =
        Helpers.applyFuncs edits
          { color: ""
          , width: 0.0
          , dotGrid: false
          , dashed: []
          }

  color =
    if config.color == "" then
      if config.dotGrid then Helpers.darkGray else Helpers.gray
    else
      config.color

  width =
    if config.width == 0.0 then
      if config.dotGrid then 0.5 else 1.0
    else
          config.width

  toXGrid vs p v =
    if elem v vs.xAxis then
      Nothing
    else
      Just $ CS.line p [ CA.color color, CA.width width, CA.x1 v, CA.dashed config.dashed ]

  toYGrid vs p v =
    if elem v vs.yAxis then
      Nothing
    else
      Just $ CS.line p [ CA.color color, CA.width width, CA.y1 v, CA.dashed config.dashed ]

  toDot vs p x y =
    if elem x vs.xAxis || elem y vs.yAxis then
      Nothing
    else
      Just $ CS.dot p _.x _.y [ CA.color color, CA.size width, CA.circle ] { x, y }

interpolated :: forall data_
                . (data_ -> Number) 
                -> Array (Attribute CS.Interpolation) 
                -> Array (Attribute CS.Dot) 
                -> Property data_ CS.Interpolation CS.Dot
interpolated y inter = IP.property (y >>> Just) ([ CA.linear ] <> inter)

interpolatedMaybe :: forall data_
                    . (data_ -> Maybe Number) 
                    -> Array (Attribute CS.Interpolation) 
                    -> Array (Attribute CS.Dot) 
                    -> Property data_ CS.Interpolation CS.Dot
interpolatedMaybe y inter = IP.property y ([ CA.linear ] <> inter)

amongst :: forall data_ deco inter x.
            Eq data_
            => Array (CI.One data_ x) 
            -> (data_ -> Array (Attribute deco)) 
            -> Property data_ inter deco 
            -> Property data_ inter deco
amongst inQuestion func =
  IP.variation \p s i _ d ->
    let check product =
          if Item.getPropertyIndex product == p &&
             Item.getStackIndex product == s &&
             Item.getDataIndex product == i &&
             Item.getDatum product == d
          then func d else []
    in
    inQuestion >>= check


stacked :: forall data_ inter deco. Array (Property data_ inter deco) -> Property data_ inter deco
stacked = IP.stacked

type Property data_ inter deco = IP.Property data_ String inter deco


-- SERIES

series :: forall data_ msg
            . (data_ -> Number) 
            -> Array (Property data_ CS.Interpolation CS.Dot) 
            -> Array data_
            -> Element data_ msg
series toX properties data_ = seriesMap identity toX properties data_

seriesMap :: forall a data_ msg
               . (data_ -> a) 
               -> (data_ -> Number) 
               -> Array (Property data_ CS.Interpolation CS.Dot) 
               -> Array data_
               -> Element a msg
seriesMap mapData toX properties data_ =
  Indexed $ \index ->
    let
      items = Produce.toDotSeries index toX properties data_

      generalized = items >>= Many.getGenerals <#> Item.mapOne mapData

      legends_ = Legend.toDotLegends index properties

      toLimits = map Item.getLimits items
    in
    ( SeriesElement toLimits generalized legends_ $ \p ->
        H.g [ H.class_ "elm-charts__dot-series" ] (Item.toSvg p <$> items)
          # map absurd
    ) /\ (index + length (properties >>= IP.toConfigs))


-- HELPERS

type TickValue =
  { value :: Number
  , label :: String
  }


generateValues :: Int -> IS.TickType -> Maybe (Number -> String) -> C.Axis -> Array TickValue
generateValues amount tick maybeFormat axis =
  case tick of
    IS.Numbers -> toTickValues identity show (IS.generate amount IS.numbers axis)
    IS.Ints -> toTickValues toNumber show (IS.generate amount IS.ints axis)
    {-
    IS.Times zone ->
      toTickValues (toFloat << Time.posixToMillis << .timestamp) (CS.formatTime zone)
        (CS.generate amount (CS.times zone) axis)
    -}
  where
  toTickValues :: forall a. (a -> Number) -> (a -> String) -> Array a -> Array TickValue
  toTickValues toValue toString =
    map \i ->
      { value: toValue i
      , label:
          case maybeFormat of
            Just formatter -> formatter (toValue i)
            Nothing -> toString i
      }


module Pha.Chart.Internal.Svg where

import Prelude

import Data.Array (catMaybes, foldl, head, last, length, mapWithIndex, reverse, zipWith)
import Data.Int as Int
import Data.List as List
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, sin, sqrt, pi, tan)
import Data.Ord (abs)
import Data.String as S
import Data.String (joinWith, replaceAll)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (T3, (/\))
import Pha.Chart.Internal.Intervals as I 
import Pha.Chart.Internal.Commands as C
import Pha.Chart.Internal.Coordinates (Plane, Point, Position)
import Pha.Chart.Internal.Coordinates as Coord
import Pha.Chart.Internal.Helpers as Helpers
import Pha.Chart.Internal.Interpolation as Interpolation
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc, px, px')

type Container msg =
  { attrs :: Array (H.Prop msg)
  , htmlAttrs :: Array (H.Prop msg)
  , responsive :: Boolean
  , events :: Array (Event msg)
  }

type Event msg =
  { name :: String
  , handler :: Plane -> Point -> msg
  }


defaultContainer :: forall msg. Container msg
defaultContainer =
  { attrs: [ H.style "overflow" "visible" ]
  , htmlAttrs: []
  , responsive: true
  , events: []
  }

container :: forall msg. Plane -> Container msg -> Array (Html msg) -> Array (Html msg) -> Array (Html msg) -> Html msg
container plane config below chartEls above =
  H.div
    [ H.class_ "elm-charts__container"
    , H.style "position" "relative"
    ]
    [ H.div htmlAttrs (below <> [ chart ] <> above) ]
  where
  -- TODO seperate plane from container size
  -- TODO preserveAspectRatio?
  htmlAttrsDef = [ H.class_ "elm-charts__container-inner" ]

  htmlAttrsSize =
        if config.responsive then
          [ H.style "width" "100%"
          , H.style "height" "100%"
          ]
        else
          [ H.style "width" (px plane.x.length)
          , H.style "height" (px plane.y.length)
          ]

  htmlAttrs = config.htmlAttrs <> htmlAttrsDef <> htmlAttrsSize

  chart = H.svg
          (svgAttrsSize <> config.attrs)
          ([clipPathDefs] <> chartEls <> [catcher])

  svgAttrsSize =
        if config.responsive then
          [ P.viewBox 0 0 (Int.ceil plane.x.length) (Int.ceil plane.y.length)
          , H.style "display" "block"
          ]
        else
          [ P.width plane.x.length
          , P.height plane.y.length
          , H.style "display" "block"
          ]

  catcher = H.rect (chartPosition) -- ++ map toEvent config.events)

  -- toEvent event = SE.on event.name (decoder plane event.handler)

  chartPosition =
    [ P.x plane.x.marginMin
    , P.y plane.y.marginMin
    , P.width $ Coord.innerWidth plane
    , P.height $ Coord.innerHeight plane
    , P.fill "transparent"
    ]

  clipPathDefs =
    H.defs []
      [ H.clipPath
        [ P.id (Coord.toId plane) ]
        [ H.rect chartPosition ]
      ]

data TickType
  = Numbers
  | Ints
  -- | Times Time.Zone

type Tick =
  { color :: String
  , width :: Number
  , length :: Number
  , attrs :: Array (H.Prop Void)
  }

defaultTick :: Tick
defaultTick =
  { length: 5.0
  , color: "rgb(210, 210, 210)"
  , width: 1.0
  , attrs: []
  }

xTick :: forall msg. Plane -> Tick -> Point -> Html msg
xTick plane config point = tick plane config true point

yTick :: forall msg. Plane -> Tick -> Point -> Html msg
yTick plane config point = tick plane config false point

tick :: forall msg. Plane -> Tick -> Boolean -> Point -> Html msg
tick plane config isX point =
  withAttrs' config.attrs H.line
    [ H.class_ "elm-charts__tick"
    , P.stroke config.color
    , P.strokeWidth config.width
    , P.x1 $ Coord.toSVGX plane point.x
    , P.x2 $ Coord.toSVGX plane point.x + if isX then 0.0 else -config.length
    , P.y1 $ Coord.toSVGY plane point.y
    , P.y2 $ Coord.toSVGY plane point.y + if isX then config.length else 0.0
    ]


-- LINE


type Line =
  { x1 :: Maybe Number
  , x2 :: Maybe Number
  , y1 :: Maybe Number
  , y2 :: Maybe Number
  , x2Svg :: Maybe Number
  , y2Svg :: Maybe Number
  , xOff :: Number
  , yOff :: Number
  , tickLength :: Number
  , tickDirection :: Number
  , color :: String
  , width :: Number
  , dashed :: Array Number
  , opacity :: Number
  , break :: Boolean
  , flip :: Boolean
  , hideOverflow :: Boolean
  , attrs :: Array (H.Prop Void)
  }


defaultLine :: Line
defaultLine =
  { x1: Nothing
  , x2: Nothing
  , y1: Nothing
  , y2: Nothing
  , x2Svg: Nothing
  , y2Svg: Nothing
  , tickLength: 0.0
  , tickDirection: -90.0
  , xOff: 0.0
  , yOff: 0.0
  , color: "rgb(210, 210, 210)"
  , width: 1.0
  , dashed: []
  , opacity: 1.0
  , break: false
  , flip: false
  , hideOverflow: false
  , attrs: []
  }


line :: forall msg. Plane -> Line -> Html msg
line plane config =
  withAttrs' config.attrs H.path
    [ H.class_ "elm-charts__line"
    , P.fill "transparent"
    , P.stroke config.color
    , P.strokeWidth config.width
    , P.strokeOpacity config.opacity
    , P.strokeDasharray (joinWith " " $ map show config.dashed)
    , P.d (C.description plane cmds)
    , if config.hideOverflow then withinChartArea plane else H.class_ ""
    ]
  where

  x1 /\ x2 /\ y1 /\ y2 =
        case config.x1, config.x2, config.y1, config.y2, config.x2Svg, config.y2Svg of
          -- ONLY X
          Just a, Just b, Nothing, Nothing, _, _ ->
            a /\ b /\ plane.y.min /\ plane.y.min

          Just a, Nothing, Nothing, Nothing, _ , _ ->
            a /\ a /\ plane.y.min /\ plane.y.max

          Nothing, Just b, Nothing, Nothing, _, _ ->
            b /\ b /\ plane.y.min /\ plane.y.max

          -- ONLY Y
          Nothing, Nothing, Just a, Just b, _, _ ->
            plane.x.min /\ plane.x.min /\ a /\ b

          Nothing, Nothing, Just a, Nothing, _, _ ->
            plane.x.min /\ plane.x.max /\ a /\ a

          Nothing, Nothing, Nothing, Just b, _, _ ->
            plane.x.min /\ plane.x.max /\ b /\ b

          -- MIXED
          Nothing, Just c, Just a, Just b, _, _ ->
            c /\ c /\ a /\ b

          Just c, Nothing, Just a, Just b, _, _ ->
            c /\ c /\ a /\ b

          Just a, Just b, Nothing, Just c, _, _ ->
            a /\ b /\ c /\ c

          Just a, Just b, Just c, Nothing, _ , _ ->
            a /\ b /\ c /\ c

          -- ONE FULL POINT
          Just a, Nothing, Nothing, Just b, Nothing, Nothing ->
            a /\ plane.x.max /\ b /\ b

          Just a, Nothing, Nothing, Just b, Just xOff, Just yOff ->
            a /\ (a + Coord.scaleCartesianX plane xOff) /\ b /\ (b + Coord.scaleCartesianY plane yOff)

          Just a, Nothing, Nothing, Just b, Just xOff, Nothing ->
            a /\ (a + Coord.scaleCartesianX plane xOff) /\ b /\ b

          Just a, Nothing, Nothing, Just b, Nothing, Just yOff ->
            a /\ a /\ b /\ (b + Coord.scaleCartesianY plane yOff)

          Just a, Nothing, Just b, Nothing, Nothing, Nothing ->
            a /\ plane.x.max /\ b /\ b

          Just a, Nothing, Just b, Nothing, Just xOff, Just yOff ->
            a /\ (a + Coord.scaleCartesianX plane xOff) /\ b /\ (b + Coord.scaleCartesianY plane yOff)

          Just a, Nothing, Just b, Nothing, Just xOff, Nothing ->
            a /\ (a + Coord.scaleCartesianX plane xOff) /\ b /\ b

          Just a, Nothing, Just b, Nothing, Nothing, Just yOff ->
            a /\ a /\ b /\ (b + Coord.scaleCartesianY plane yOff)

          Nothing, Just a, Nothing, Just b, Nothing, Nothing ->
            a /\ plane.x.max /\ b /\ b

          Nothing, Just a, Nothing, Just b, Just xOff, Just yOff ->
            a /\ (a + Coord.scaleCartesianX plane xOff) /\ b /\ (b + Coord.scaleCartesianY plane yOff)

          Nothing, Just a, Nothing, Just b, Just xOff, Nothing ->
            a /\ (a + Coord.scaleCartesianX plane xOff) /\ b /\ b

          Nothing, Just a, Nothing, Just b, Nothing, Just yOff ->
            a /\ a /\ b /\ (b + Coord.scaleCartesianY plane yOff)

          Nothing, Just a, Just b, Nothing, Nothing, Nothing ->
            a /\ plane.x.max /\ b /\ b

          Nothing, Just a, Just b, Nothing, Just xOff, Just yOff ->
            a /\ (a + Coord.scaleCartesianX plane xOff) /\ b /\ (b + Coord.scaleCartesianY plane yOff)

          Nothing, Just a , Just b, Nothing, Just xOff, Nothing ->
            a /\ (a + Coord.scaleCartesianX plane xOff) /\ b /\ b

          Nothing, Just a, Just b, Nothing, Nothing, Just yOff ->
            a /\ a /\ b /\ (b + Coord.scaleCartesianY plane yOff)

          -- NEITHER
          Nothing, Nothing, Nothing, Nothing, _, _ ->
            plane.x.min /\ plane.x.max /\ plane.y.min /\ plane.y.max

          _, _, _, _, _, _ ->
            fromMaybe plane.x.min config.x1 /\ fromMaybe plane.x.max config.x2
            /\ fromMaybe plane.y.min config.y1 /\ fromMaybe plane.y.max config.y2

  angle = pi * config.tickDirection / 180.0

  (tickOffsetX /\ tickOffsetY) =
    if config.tickLength > 0.0 then
      (lengthInCartesianX plane (cos angle * config.tickLength)
      /\ lengthInCartesianY plane (sin angle * config.tickLength)
      )
    else
      (0.0 /\  0.0)

  x1_ = x1 + lengthInCartesianX plane config.xOff
  x2_ = x2 + lengthInCartesianX plane config.xOff
  y1_ = y1 - lengthInCartesianY plane config.yOff
  y2_ = y2 - lengthInCartesianY plane config.yOff

  cmds =
    if config.flip then
      (if config.tickLength > 0.0
        then [ C.Move (x2_ + tickOffsetX) (y2_ + tickOffsetY), C.Line x2_ y2_ ]
        else [ C.Move x2_ y2_ ])
      <>
      (if config.break
        then [ C.Line x2_ y1_, C.Line x1_ y1_ ]
        else [ C.Line x1_ y1_ ])
      <>
      (if config.tickLength > 0.0
        then [ C.Line (x1_ + tickOffsetX) (y1_ + tickOffsetY) ]
        else [])

    else
      (if config.tickLength > 0.0
        then [ C.Move (x1_ + tickOffsetX) (y1_ + tickOffsetY), C.Line x1_ y1_ ]
        else [ C.Move x1_ y1_ ])
      <>
      (if config.break
        then [ C.Line x1_ y2_, C.Line x2_ y2_ ]
        else [ C.Line x2_ y2_ ])
      <>
        (if config.tickLength > 0.0
        then [ C.Line (x2_ + tickOffsetX) (y2_ + tickOffsetY) ]
        else [])


type Rect =
  { x1 :: Maybe Number
  , x2 :: Maybe Number
  , y1 :: Maybe Number
  , y2 :: Maybe Number
  , color :: String
  , border :: String
  , borderWidth :: Number
  , opacity :: Number
  , hideOverflow :: Boolean
  , attrs :: Array (H.Prop Void)
  }


defaultRect :: Rect
defaultRect =
  { x1: Nothing
  , x2: Nothing
  , y1: Nothing
  , y2: Nothing
  , color: "rgba(210, 210, 210, 0.5)"
  , border: "rgba(210, 210, 210, 1)"
  , borderWidth: 1.0
  , opacity: 1.0
  , hideOverflow: false
  , attrs: []
  }

rect :: forall msg. Plane -> Rect -> Html msg
rect plane config =
  withAttrs' config.attrs H.path
    [ H.class_ "elm-charts__rect"
    , P.fill config.color
    , P.fillOpacity config.opacity
    , P.stroke config.border
    , P.strokeWidth config.borderWidth
    , P.d (C.description plane cmds)
    , if config.hideOverflow then withinChartArea plane else H.class_ ""
    ]
  where
  x1_ /\ x2_ /\ y1_ /\ y2_ =
    case config.x1, config.x2, config.y1, config.y2 of
      -- ONLY X
      Just a, Just b, Nothing, Nothing ->
        a /\ b /\ plane.y.min /\ plane.y.max

      Just a, Nothing, Nothing, Nothing ->
        a /\ a /\ plane.y.min /\ plane.y.max

      Nothing, Just b, Nothing, Nothing ->
        b /\ b /\ plane.y.min /\ plane.y.max

      -- ONLY Y
      Nothing, Nothing, Just a, Just b ->
        plane.x.min /\ plane.x.min /\ a /\ b

      Nothing, Nothing, Just a, Nothing ->
        plane.x.min /\ plane.x.max /\ a /\ a

      Nothing, Nothing, Nothing, Just b ->
        plane.x.min /\ plane.x.max /\ b /\ b

      -- MIXED
      Nothing, Just c, Just a, Just b ->
        c /\ c /\ a /\ b

      Just c, Nothing, Just a, Just b ->
        c /\ c  /\ a /\ b

      Just a, Just b, Nothing, Just c ->
        a /\ b /\ c /\ c

      Just a, Just b, Just c, Nothing ->
        a /\ b /\ c /\ c

      -- NEITHER
      Nothing, Nothing, Nothing, Nothing  ->
        plane.x.min /\ plane.x.max /\ plane.y.min /\ plane.y.max

      _, _, _, _ ->
        fromMaybe plane.x.min config.x1
        /\ fromMaybe plane.x.max config.x2
        /\ fromMaybe plane.y.min config.y1
        /\ fromMaybe plane.y.max config.y2

  cmds =
    [ C.Move x1_ y1_
    , C.Line x1_ y1_
    , C.Line x2_ y1_
    , C.Line x2_ y2_
    , C.Line x1_ y2_
    , C.Line x1_ y1_
    ]


-- LEGEND


type Legends msg =
  { alignment :: Alignment
  , anchor :: Maybe Anchor
  , xOff :: Number
  , yOff :: Number
  , spacing :: Number
  , background :: String
  , border :: String
  , borderWidth :: Number
  , htmlAttrs :: Array (H.Prop msg)
  }


data Alignment = Row | Column

data Anchor = End | Start | Middle


defaultLegends :: forall msg. Legends msg
defaultLegends =
  { alignment: Row
  , anchor: Nothing
  , spacing: 10.0
  , xOff: 0.0
  , yOff: 0.0
  , background: ""
  , border: ""
  , borderWidth: 0.0
  , htmlAttrs: []
  }

legendsAt :: forall msg. Plane -> Number -> Number -> Legends msg -> Array (Html msg) -> Html msg
legendsAt plane x y config children =
  positionHtml plane x y config.xOff (-config.yOff)
    (anchorAttrs <> alignmentAttrs <> otherAttrs <> config.htmlAttrs)
    ([H.elem "style" [] [ H.text paddingStyle ]] <> children)
  where
  alignmentAttrs /\ direction =
    case config.alignment of
      Row ->
        ( [ H.style "display" "flex"
          , H.style "align-items" "center"
          ]
        /\ "right"
        )

      Column ->
        ( [ H.style "display" "flex"
          , H.style "flex-direction" "column"
          , H.style "align-items" "baseline"
          ]
        /\ "bottom"
        )

  otherAttrs =
    [ H.class_ "elm-charts__legends"
    , H.style "background" config.background
    , H.style "border-color" config.border
    , H.style "border-width" $ px config.borderWidth
    , H.style "border-style" "solid"
    ]

  paddingStyle =
    """ .elm-charts__legends .elm-charts__legend {
      margin-""" <> direction <> ":" <> px config.spacing <> """;
    }
    .elm-charts__legends .elm-charts__legend:last-child {
      margin-""" <> direction <> """: 0px;
    }
    """
  anchorAttrs =
    case config.anchor of
      Nothing -> [ H.style "transform" "translate(-0%, 0%)" ]
      Just End -> [ H.style "transform" "translate(-100%, 0%)" ]
      Just Start -> [ H.style "transform" "translate(-0%, 0%)" ]
      Just Middle -> [ H.style "transform" "translate(-50%, 0%)" ]


-- LEGEND


type Legend msg =
  { xOff :: Number
  , yOff :: Number
  , width :: Number
  , height :: Number
  , fontSize :: Maybe Int
  , color :: String
  , spacing :: Number
  , title :: String
  , htmlAttrs :: Array (H.Prop msg)
  }

defaultBarLegend :: forall msg. Legend msg
defaultBarLegend =
  { xOff: 0.0
  , yOff: 0.0
  , width: 10.0
  , height: 10.0
  , fontSize: Nothing
  , color: "#808BAB"
  , spacing: 10.0
  , title: ""
  , htmlAttrs: []
  }

defaultLineLegend :: forall msg. Legend msg
defaultLineLegend =
  { xOff: 0.0
  , yOff: 0.0
  , width: 30.0
  , height: 16.0
  , fontSize: Nothing
  , color: "#808BAB"
  , spacing: 10.0
  , title: ""
  , htmlAttrs: []
  }


barLegend :: forall msg. Legend msg -> Bar ->  Html msg
barLegend config barConfig =
  H.div
    ([ H.class_ "elm-charts__legend"
    , H.style "display" "flex"
    , H.style "align-items" "center"
    ] <> config.htmlAttrs)
    [ container fakePlane defaultContainer { responsive = false } []
        [ bar fakePlane barConfig { x1: 0.0, y1: 0.0, x2: 10.0, y2: 10.0 } ]
        []
    , H.div
        [ fontStyle
        , H.style "margin-left" $ px config.spacing
        ]
        [ H.text config.title ]
    ]
  where
  fakePlane =
    { x: { length: config.width, marginMin: 0.0, marginMax: 0.0, dataMin: 0.0, dataMax: 10.0, min: 0.0, max: 10.0 }
    , y: { length: config.height, marginMin: 0.0, marginMax: 0.0, dataMin: 0.0, dataMax: 10.0, min: 0.0, max: 10.0 }
    }

  fontStyle =
    case config.fontSize of
      Just size_ -> H.style "font-size" (px' size_)
      Nothing -> H.noProp


lineLegend :: forall msg. Legend msg -> Interpolation -> Dot -> Html msg
lineLegend config interConfig dotConfig =
  H.div
    ([ H.class_ "elm-charts__legend"
    , H.style "display" "flex"
    , H.style "align-items" "center"
    ] <> config.htmlAttrs)
    [ container fakePlane defaultContainer { responsive = false } []
        [ interpolation fakePlane _.x (Just <<< _.y) interConfig [ {x: 0.0, y: 5.0}, {x: 10.0, y: 5.0} ]
        , area fakePlane _.x Nothing (Just <<< _.y) interConfig [ {x: 0.0, y: 5.0}, {x: 10.0, y: 5.0} ]
        , dot fakePlane _.x _.y dotConfig {x: 5.0, y: 5.0}
        ]
        []
    , H.div
        [ fontStyle
        , H.style "margin-left" (px config.spacing)
        ]
        [ H.text config.title ]
    ]
  where
  topMargin =
    case dotConfig.shape of
      Just shape -> toRadius dotConfig.size shape
      Nothing -> 0.0

  bottomMargin = if interConfig.opacity == 0.0 then topMargin else 0.0

  fakePlane =
    { x: { length: config.width, marginMin: 0.0, marginMax: 0.0, dataMin: 0.0, dataMax: 10.0, min: 0.0, max: 10.0 }
    , y: { length: config.height, marginMin: 0.0, marginMax: 0.0, dataMin: 0.0, dataMax: 10.0, min: 0.0, max: 10.0 }
    }

  fontStyle =
    case config.fontSize of
      Just size_ -> H.style "font-size" (px' size_)
      Nothing -> H.class_ ""



-- LABEL


type Label =
  { xOff :: Number
  , yOff :: Number
  , border :: String
  , borderWidth :: Number
  , fontSize :: Maybe Int
  , color :: String
  , anchor :: Maybe Anchor
  , rotate :: Number
  , uppercase :: Boolean
  , hideOverflow :: Boolean
  , attrs :: Array (H.Prop Void)
  , ellipsis :: Maybe { width :: Number, height :: Number }
  }


defaultLabel :: Label
defaultLabel =
  { xOff: 0.0
  , yOff: 0.0
  , border: "white"
  , borderWidth: 0.0
  , fontSize: Nothing
  , color: "#808BAB"
  , anchor: Nothing
  , rotate: 0.0
  , uppercase: false
  , hideOverflow: false
  , attrs: []
  , ellipsis: Nothing
  }


label :: forall msg. Plane -> Label -> Array (Html msg) -> Point -> Html msg
label plane config inner point =
  case config.ellipsis of
    Nothing ->
      let
        fontStyle =
            case config.fontSize of
              Just size_ -> H.style "font-size" (px' size_)
              Nothing -> H.noProp

        anchorStyle =
            case config.anchor of
              Nothing -> H.style "text-anchor" "middle"
              Just End -> H.style "text-anchor" "end"
              Just Start -> H.style "text-anchor" "start"
              Just Middle -> H.style "text-anchor" "middle"

        uppercaseStyle =
            if config.uppercase then H.style "text-transform" "uppercase" else H.noProp

        withOverflowWrap el =
            if config.hideOverflow then
              H.g [ withinChartArea plane ] [ el ]
            else
              el
      in
      withOverflowWrap $
        withAttrs config.attrs H.text_
          [ H.class_ "elm-charts__label"
          , P.stroke config.border
          , P.strokeWidth config.borderWidth
          , P.fill config.color
          , position plane (-config.rotate) point.x point.y config.xOff config.yOff
          , H.style "pointer-events" "none"
          , fontStyle
          , anchorStyle
          , uppercaseStyle
          ]
          [ H.tspan [] inner ]

    Just ellipsis ->
      let fontStyle =
            case config.fontSize of
              Just size_ -> H.style "font-size" (px' size_)
              Nothing -> H.class_ ""

          xOffWithAnchor =
            case config.anchor of
              Nothing -> config.xOff - ellipsis.width / 2.0
              Just End -> config.xOff - ellipsis.width
              Just Start -> config.xOff
              Just Middle -> config.xOff - ellipsis.width / 2.0

          anchorStyle =
            case config.anchor of
              Nothing -> H.style "text-align" "center"
              Just End -> H.style "text-align" "right"
              Just Start -> H.style "text-align" "left"
              Just Middle -> H.style "text-align" "center"

          uppercaseStyle =
            if config.uppercase then H.style "text-transform" "uppercase" else H.noProp

          withOverflowWrap el =
            if config.hideOverflow then
              H.g [ withinChartArea plane ] [ el ]
            else
              el
      in
      withOverflowWrap $
        withAttrs config.attrs H.foreignObject
          [ H.class_ "elm-charts__label"
          , H.class_ "elm-charts__html-label"
          , P.width ellipsis.width
          , P.height ellipsis.height
          , position plane (-config.rotate) point.x point.y xOffWithAnchor (config.yOff - 10.0)
          ]
          [ H.div
              [ H.attr "xmlns" "http://www.w3.org/1999/xhtml"
              , H.style "white-space" "nowrap"
              , H.style "overflow" "hidden"
              , H.style "text-overflow" "ellipsis"
              , H.style "height" "100%"
              , H.style "pointer-events" "none"
              , H.style "color" config.color
              , fontStyle
              , uppercaseStyle
              , anchorStyle
              ]
              inner
          ]



-- ARROW

type Arrow =
  { xOff :: Number
  , yOff :: Number
  , color :: String
  , width :: Number
  , length :: Number
  , rotate :: Number
  , attrs :: Array (H.Prop Void)
  }


defaultArrow :: Arrow
defaultArrow =
  { xOff: 0.0
  , yOff: 0.0
  , color: "rgb(210, 210, 210)"
  , width: 4.0
  , length: 7.0
  , rotate: 0.0
  , attrs: []
  }

arrow :: forall msg. Plane -> Arrow -> Point -> Html msg
arrow plane config point =
  let points_ =
        "0,0 " <> show config.length <> "," <> show config.width <> " 0, " <> show (config.width * 2.0)

      commands =
        "rotate(" <> show config.rotate <> ") translate(0 " <> show (-config.width) <> ") "
  in
  H.g
    [ H.class_ "elm-charts__arrow"
    , position plane 0.0 point.x point.y config.xOff config.yOff
    ]
    [ withAttrs' config.attrs H.polygon
        [ P.fill config.color
        , P.points points_
        , P.transform commands
        ]
    ]



-- BAR

type Bar =
  { roundTop :: Number
  , roundBottom :: Number
  , color :: String
  , border :: String
  , borderWidth :: Number
  , opacity :: Number
  , design :: Maybe Design
  , attrs :: Array (H.Prop Void)
  , highlight :: Number
  , highlightWidth :: Number
  , highlightColor :: String
  }


data Design
  = Striped (Array (Pattern -> Pattern))
  | Dotted (Array (Pattern -> Pattern))
  | Gradient (Array String)

type Pattern =
  { color :: String
  , width :: Number
  , spacing :: Number
  , rotate :: Number
  }


defaultBar :: Bar
defaultBar =
  { roundTop: 0.0
  , roundBottom: 0.0
  , border: "white"
  , borderWidth: 0.0
  , color: Helpers.pink
  , opacity: 1.0
  , design: Nothing
  , attrs: []
  , highlight: 0.0
  , highlightWidth: 10.0
  , highlightColor: ""
  }


bar :: forall msg. Plane -> Bar -> Position -> Html msg
bar plane config point =
  case config.design of
    Nothing -> viewAuraBar config.color
    Just design ->
      let patternDefs /\ fill = toPattern config.color design
      in
      H.g
        [ H.class_ "elm-charts__bar-with-pattern" ]
        [ patternDefs
        , viewAuraBar fill
        ]

  where
  highlightColor = if config.highlightColor == "" then config.color else config.highlightColor

  borderWidthCarX = Coord.scaleCartesianX plane (config.borderWidth / 2.0)
  borderWidthCarY = Coord.scaleCartesianY plane (config.borderWidth / 2.0)

  pos =
    { x1: min point.x1 point.x2 + borderWidthCarX
    , x2: max point.x1 point.x2 - borderWidthCarX
    , y1: min point.y1 point.y2 + borderWidthCarY
    , y2: max point.y1 point.y2 - borderWidthCarY
    }

  highlightWidthCarX = borderWidthCarX + Coord.scaleCartesianX plane (config.highlightWidth / 2.0)
  highlightWidthCarY = borderWidthCarY + Coord.scaleCartesianY plane (config.highlightWidth / 2.0)

  highlightPos =
    { x1: pos.x1 - highlightWidthCarX
    , x2: pos.x2 + highlightWidthCarX
    , y1: pos.y1 - highlightWidthCarY
    , y2: pos.y2 + highlightWidthCarY
    }

  width = abs (pos.x2 - pos.x1)
  roundingTop = Coord.scaleSVGX plane width * 0.5 * (clamp 0.0 1.0 config.roundTop)
  roundingBottom = Coord.scaleSVGX plane width * 0.5 * (clamp 0.0 1.0 config.roundBottom)
  radiusTopX = Coord.scaleCartesianX plane roundingTop
  radiusTopY = Coord.scaleCartesianY plane roundingTop
  radiusBottomX = Coord.scaleCartesianX plane roundingBottom
  radiusBottomY = Coord.scaleCartesianY plane roundingBottom

  height = abs (pos.y2 - pos.y1)
  roundTop /\ roundBottom =
    if height - radiusTopY * 0.8 - radiusBottomY * 0.8 <= 0.0 || width - radiusTopX * 0.8 - radiusBottomX * 0.8 <= 0.0 then
      0.0 /\ 0.0
    else
      config.roundTop /\ config.roundBottom

  commands /\ highlightCommands =
    if pos.y1 == pos.y2 then
      [] /\ []
    else
      case roundTop > 0.0, roundBottom > 0.0 of
        false, false ->
          [ C.Move pos.x1 pos.y1
          , C.Line pos.x1 pos.y2
          , C.Line pos.x2 pos.y2
          , C.Line pos.x2 pos.y1
          , C.Line pos.x1 pos.y1
          ]
          /\
          [ C.Move highlightPos.x1 pos.y1
          , C.Line highlightPos.x1 highlightPos.y2
          , C.Line highlightPos.x2 highlightPos.y2
          , C.Line highlightPos.x2 pos.y1
          -- ^ outer
          , C.Line pos.x2 pos.y1
          , C.Line pos.x2 pos.y2
          , C.Line pos.x1 pos.y2
          , C.Line pos.x1 pos.y1
          ]
        true, false ->
          [ C.Move pos.x1 pos.y1
          , C.Line pos.x1 (pos.y2 - radiusTopY)
          , C.Arc roundingTop roundingTop (-45) false true (pos.x1 + radiusTopX) pos.y2
          , C.Line (pos.x2 - radiusTopX) pos.y2
          , C.Arc roundingTop roundingTop (-45) false true pos.x2 (pos.y2 - radiusTopY)
          , C.Line pos.x2 pos.y1
          , C.Line pos.x1 pos.y1
          ]
          /\
          [ C.Move highlightPos.x1 pos.y1
          , C.Line highlightPos.x1 (highlightPos.y2 - radiusTopY)
          , C.Arc roundingTop roundingTop (-45) false true (highlightPos.x1 + radiusTopX) highlightPos.y2
          , C.Line (highlightPos.x2 - radiusTopX) highlightPos.y2
          , C.Arc roundingTop roundingTop (-45) false true highlightPos.x2 (highlightPos.y2 - radiusTopY)
          , C.Line highlightPos.x2 pos.y1
          -- ^ outer
          , C.Line pos.x2 pos.y1
          , C.Line pos.x2 (pos.y2 - radiusTopY)
          , C.Arc roundingTop roundingTop (-45) false false (pos.x2 - radiusTopX) pos.y2
          , C.Line (pos.x1 + radiusTopX) pos.y2
          , C.Arc roundingTop roundingTop (-45) false false pos.x1 (pos.y2 - radiusTopY)
          , C.Line pos.x1 pos.y1
          ]
        false, true ->
          [ C.Move (pos.x1 + radiusBottomX) pos.y1
          , C.Arc roundingBottom roundingBottom (-45) false true pos.x1 (pos.y1 + radiusBottomY)
          , C.Line pos.x1 pos.y2
          , C.Line pos.x2 pos.y2
          , C.Line pos.x2 (pos.y1 + radiusBottomY)
          , C.Arc roundingBottom roundingBottom (-45) false true (pos.x2 - radiusBottomX) pos.y1
          , C.Line (pos.x1 + radiusBottomX) pos.y1
          ]
          /\
          [ C.Move (highlightPos.x1 + radiusBottomX) highlightPos.y1
          , C.Arc roundingBottom roundingBottom (-45) false true highlightPos.x1 (highlightPos.y1 + radiusBottomY)
          , C.Line highlightPos.x1 highlightPos.y2
          , C.Line highlightPos.x2 highlightPos.y2
          , C.Line highlightPos.x2 (highlightPos.y1 + radiusBottomY)
          , C.Arc roundingBottom roundingBottom (-45) false true (highlightPos.x2 - radiusBottomX) highlightPos.y1
          , C.Line (highlightPos.x1 + radiusBottomX) highlightPos.y1
          -- ^ outer
          , C.Line (pos.x2 - radiusBottomX) pos.y1
          , C.Arc roundingBottom roundingBottom (-45) false false pos.x2 (pos.y1 + radiusBottomY)
          , C.Line pos.x2 pos.y2
          , C.Line pos.x1 pos.y2
          , C.Line pos.x1 (pos.y1 + radiusBottomY)
          , C.Line pos.x2 pos.y1
          ]
        true, true ->
          [ C.Move (pos.x1 + radiusBottomX) pos.y1
          , C.Arc roundingBottom roundingBottom (-45) false true pos.x1 (pos.y1 + radiusBottomY)
          , C.Line pos.x1 (pos.y2 - radiusTopY)
          , C.Arc roundingTop roundingTop (-45) false true (pos.x1 + radiusTopX) pos.y2
          , C.Line (pos.x2 - radiusTopX) pos.y2
          , C.Arc roundingTop roundingTop (-45) false true pos.x2 (pos.y2 - radiusTopY)
          , C.Line pos.x2 (pos.y1 + radiusBottomY)
          , C.Arc roundingBottom roundingBottom (-45) false true (pos.x2 - radiusBottomX) pos.y1
          , C.Line (pos.x1 + radiusBottomX) pos.y1
          ]
          /\
          [ C.Move (highlightPos.x1 + radiusBottomX) highlightPos.y1
          , C.Arc roundingBottom roundingBottom (-45) false true highlightPos.x1 (highlightPos.y1 + radiusBottomY)
          , C.Line highlightPos.x1 (highlightPos.y2 - radiusTopY)
          , C.Arc roundingTop roundingTop (-45) false true (highlightPos.x1 + radiusTopX) highlightPos.y2
          , C.Line (highlightPos.x2 - radiusTopX) highlightPos.y2
          , C.Arc roundingTop roundingTop (-45) false true highlightPos.x2 (highlightPos.y2 - radiusTopY)
          , C.Line highlightPos.x2 (highlightPos.y1 + radiusBottomY)
          , C.Arc roundingBottom roundingBottom (-45) false true (highlightPos.x2 - radiusBottomX) highlightPos.y1
          , C.Line (highlightPos.x1 + radiusBottomX) highlightPos.y1
          -- ^ outer
          , C.Line (pos.x2 - radiusBottomX) pos.y1
          , C.Arc roundingBottom roundingBottom (-45) false false pos.x2 (pos.y1 + radiusBottomY)
          , C.Line pos.x2 (pos.y2 - radiusTopY)
          , C.Arc roundingTop roundingTop (-45) false false (pos.x2 - radiusTopX) pos.y2
          , C.Line (pos.x1 + radiusTopX) pos.y2
          , C.Arc roundingTop roundingTop (-45) false false pos.x1 (pos.y2 - radiusTopY)
          , C.Line pos.x1 (pos.y1 + radiusBottomY)
          , C.Line pos.x2 pos.y1
          ]

  viewAuraBar fill =
    if config.highlight == 0.0 then
      viewBar fill config.opacity config.border config.borderWidth 1.0 commands
    else
      H.g
        [ H.class_ "elm-charts__bar-with-highlight" ]
        [ viewBar highlightColor config.highlight "transparent" 0.0 0.0 highlightCommands
        , viewBar fill config.opacity config.border config.borderWidth 1.0 commands
        ]

  viewBar fill fillOpacity border borderWidth strokeOpacity cmds =
    withAttrs' config.attrs H.path
      [ H.class_ "elm-charts__bar"
      , P.fill fill
      , P.fillOpacity fillOpacity
      , P.stroke border
      , P.strokeWidth borderWidth
      , P.strokeOpacity strokeOpacity
      , P.d (C.description plane cmds)
      , withinChartArea plane
      ]

-- POSITIONING

position :: forall msg. Plane -> Number -> Number -> Number -> Number -> Number -> H.Prop msg
position plane rotation x_ y_ xOff_ yOff_ =
  P.transform $
    "translate(" <> show (Coord.toSVGX plane x_ + xOff_) 
    <> "," <> show (Coord.toSVGY plane y_ + yOff_) 
    <> ") rotate(" <> show rotation <> ")"

positionHtml :: forall msg. Plane -> Number -> Number -> Number -> Number -> Array (H.Prop msg) -> Array (Html msg) -> Html msg
positionHtml plane x y xOff yOff attrs content =
  H.div (posititonStyles <> attrs) content
  where
  xPercentage = (Coord.toSVGX plane x + xOff) / plane.x.length
  yPercentage = (Coord.toSVGY plane y - yOff) / plane.y.length

  posititonStyles =
    [ H.style "left" $ pc xPercentage
    , H.style "top" $ pc yPercentage
    , H.style "margin-right" "-400px"
    , H.style "position" "absolute"
    ]


-- SERIES

type Interpolation =
  { method :: Maybe Method
  , color :: String
  , width :: Number
  , opacity :: Number
  , design :: Maybe Design
  , dashed :: Array Number
  , attrs :: Array (H.Prop Void)
  }


data Method = Linear | Monotone | Stepped


defaultInterpolation :: Interpolation
defaultInterpolation =
  { method: Nothing
  , color: Helpers.pink
  , width: 1.0
  , opacity: 0.0
  , design: Nothing
  , dashed: []
  , attrs: []
  }

interpolation :: forall d msg. Plane -> (d -> Number) -> (d -> Maybe Number) -> Interpolation -> Array d -> Html msg
interpolation plane toX toY config data_ =
  H.maybe config.method \method ->
    H.g [ H.class_ "elm-charts__interpolation-sections" ] $
      view <$> toCommands method toX toY data_
  where
  view (first /\ cmds /\ _ ) =
    withAttrs' config.attrs H.path
      [ H.class_ "elm-charts__interpolation-section"
      , P.fill "transparent"
      , P.stroke config.color
      , P.strokeDasharray (joinWith " " $ show <$> config.dashed)
      , P.strokeWidth config.width
      , P.d (C.description plane ([C.Move first.x first.y] <> cmds))
      , withinChartArea plane
      ]

defaultArea :: Interpolation
defaultArea =
  { method: Nothing
  , color: Helpers.pink
  , width: 1.0
  , opacity: 0.2
  , design: Nothing
  , dashed: []
  , attrs: []
  }

area :: forall d msg
        . Plane
        -> (d -> Number)
        -> Maybe (d -> Maybe Number) 
        -> (d -> Maybe Number) 
        -> Interpolation 
        -> Array d
        -> Html msg
area plane toX toY2M toY config data_ =
  H.when (config.opacity > 0.0) \_ ->
    H.maybe config.method \method ->
      H.g [ H.class_ "elm-charts__area-sections" ] $
        case toY2M of
          Nothing -> [patternDefs] <> map withoutUnder (toCommands method toX toY data_)
          Just toY2 -> [patternDefs] <> zipWith withUnder (toCommands method toX toY2 data_) (toCommands method toX toY data_)

  where
  patternDefs /\ fill =
    case config.design of
      Nothing -> H.empty /\ config.color
      Just design -> toPattern config.color design

  view cmds =
    withAttrs' config.attrs H.path
      [ H.class_ "elm-charts__area-section"
      , P.fill fill
      , P.fillOpacity config.opacity
      , P.strokeWidth 0.0
      , P.fillRule "evenodd"
      , P.d (C.description plane cmds)
      , withinChartArea plane
      ]

  withoutUnder ( first /\ cmds /\ end ) =
    view $
      [ C.Move first.x 0.0, C.Line first.x first.y ] <> cmds <> [ C.Line end.x 0.0 ]

  withUnder ( firstBottom /\ cmdsBottom /\ _ ) ( firstTop /\ cmdsTop /\ endTop ) =
    view $
      [ C.Move firstBottom.x firstBottom.y, C.Line firstTop.x firstTop.y ] <> cmdsTop <>
      [ C.Move firstBottom.x firstBottom.y ] <> cmdsBottom <> [ C.Line endTop.x endTop.y ]

toCommands :: forall d
              . Method 
              -> (d -> Number) 
              -> (d -> Maybe Number)
              -> Array d
              -> Array (T3 Point (Array C.Command) Point)
toCommands method toX toY data_ =
  catMaybes $ zipWith toSets points commands
  where

  fold acc datum_  =
        case toY datum_ of
          Just y_ ->
            case acc of
              latest : rest -> (latest <> [ { x: toX datum_, y: y_ } ]) : rest
              Nil -> [ { x: toX datum_, y: y_ } ] : acc

          Nothing -> [] : acc

  points :: Array (Array Point)
  points = reverse $ List.toUnfoldable $ foldl fold Nil data_

  commands =
    case method of
      Linear -> Interpolation.linear points
      Monotone -> Interpolation.monotone points
      Stepped -> Interpolation.stepped points

  toSets :: Array Point -> Array C.Command -> Maybe (T3 Point (Array C.Command) Point)
  toSets ps cmds = withBorder ps \first last_ -> first /\ cmds /\ last_


-- DOTS

type Dot =
  { color :: String
  , opacity :: Number
  , size :: Number
  , border :: String
  , borderWidth :: Number
  , highlight :: Number
  , highlightWidth :: Number
  , highlightColor :: String
  , shape :: Maybe Shape
  , hideOverflow :: Boolean
  }

data Shape
  = Circle
  | Triangle
  | Square
  | Diamond
  | Cross
  | Plus

defaultDot :: Dot
defaultDot =
  { color: Helpers.pink
  , opacity: 1.0
  , size: 6.0
  , border: ""
  , borderWidth: 0.0
  , highlight: 0.0
  , highlightWidth: 5.0
  , highlightColor: ""
  , shape: Nothing
  , hideOverflow: false
  }

dot :: forall d msg. Plane -> (d -> Number) -> (d -> Number) -> Dot -> d -> Html msg
dot plane toX toY config datum_ =
  H.when showDot \_ ->
    case config.shape of
      Nothing -> H.empty

      Just Circle ->
        view H.circle (config.highlightWidth / 2.0) $ \off ->
          let radius = sqrt (area_ / pi) in
          [ P.cx x_
          , P.cy y_
          , P.r (radius + off)
          ]

      Just Triangle ->
        view H.path config.highlightWidth $ \off ->
          [ P.d (trianglePath area_ off x_ y_) ]

      Just Square ->
        view H.rect config.highlightWidth $ \off ->
          let side = sqrt area_
              sideOff = side + off
          in
          [ P.x $ x_ - sideOff / 2.0
          , P.y $ y_ - sideOff / 2.0
          , P.width sideOff
          , P.height sideOff
          ]

      Just Diamond ->
        view H.rect config.highlightWidth $ \off ->
          let side = sqrt area_
              sideOff = side + off
          in
          [ P.x $ x_ - sideOff / 2.0
          , P.y $ y_ - sideOff / 2.0
          , P.width sideOff
          , P.height sideOff
          , P.transform $ "rotate(45 " <> show x_ <> " " <> show y_ <> ")"
          ]

      Just Cross ->
        view H.path config.highlightWidth $ \off ->
          [ P.d (plusPath area_ off x_ y_)
          , P.transform $ "rotate(45 " <> show x_ <> " " <> show y_ <> ")"
          ]

      Just Plus ->
        view H.path config.highlightWidth $ \off ->
          [ P.d (plusPath area_ off x_ y_) ]

  where
  xOrg = toX datum_
  yOrg = toY datum_
  x_ = Coord.toSVGX plane xOrg
  y_ = Coord.toSVGY plane yOrg
  area_ = 2.0 * pi * config.size
  highlightColor = if config.highlightColor == "" then config.color else config.highlightColor
  showDot = isWithinPlane plane xOrg yOrg || config.hideOverflow

  styleAttrs =
    [ P.stroke (if config.border == "" then config.color else config.border)
    , P.strokeWidth config.borderWidth
    , P.fillOpacity config.opacity
    , P.fill config.color
    , H.class_ "elm-charts__dot"
    , if config.hideOverflow then withinChartArea plane else H.noProp
    ]

  highlightAttrs =
    [ P.stroke highlightColor
    , P.strokeWidth config.highlightWidth
    , P.strokeOpacity config.highlight
    , P.fill "transparent"
    , H.class_ "elm-charts__dot-highlight"
    ]

  view toEl highlightOff toAttrs =
    if config.highlight > 0.0 then
      H.g
        [ H.class_ "elm-charts__dot-container" ]
        [ toEl (toAttrs highlightOff <> highlightAttrs)
        , toEl (toAttrs 0.0 <> styleAttrs)
        ]
    else
      toEl (toAttrs 0.0 <> styleAttrs)


toRadius :: Number -> Shape -> Number
toRadius size_ shape =
  let area_ = 2.0 * pi * size_ in
  case shape of
    Circle   -> sqrt (area_ / pi)
    Triangle -> let side = sqrt $ area_ * 4.0 / (sqrt 3.0) in (sqrt 3.0) * side
    Square   -> sqrt area_ / 2.0
    Diamond  -> sqrt area_ / 2.0
    Cross    -> sqrt (area_ / 4.0)
    Plus     -> sqrt (area_ / 4.0)

trianglePath :: Number -> Number -> Number -> Number -> String
trianglePath area_ off x_ y_ =
  joinWith " "
    [ "M" <> show x_ <> " " <> show (y_ - fromMiddle)
    , "l" <> show (-side / 2.0) <> " " <> show height
    , "h" <> show side
    , "z"
    ]
  where
  side = sqrt (area_ * 4.0 / sqrt 3.0) + off * sqrt 3.0
  height = (sqrt 3.0) * side / 2.0
  fromMiddle = height - tan (pi / 6.0) * side / 2.0


plusPath :: Number -> Number -> Number -> Number ->  String
plusPath area_ off x_ y_ =
  joinWith " "
    [ "M" <> show (x_ - r6) <> " " <> show (y_ - r3 - r6 + off)
    , "v" <> show (r3 - off)
    , "h" <> show (-r3 + off)
    , "v" <> show r3
    , "h" <> show (r3 - off)
    , "v" <> show (r3 - off)
    , "h" <> show r3
    , "v" <> show (-r3 + off)
    , "h" <> show (r3 - off)
    , "v" <> show (-r3)
    , "h" <> show (-r3 + off)
    , "v" <> show (-r3 + off)
    , "h" <> show (-r3)
    , "v" <> show (r3 - off)
    ]
  where
  side = sqrt (area_ / 4.0) + off
  r3 = side
  r6 = side / 2.0

-- PATTERN


toPattern :: forall msg. String -> Design -> Tuple (Html msg) String
toPattern defaultColor design =
  patternDefs /\ ("url(#" <> patternId <> ")")
  where
  toPatternId props =
        replaceAll (S.Pattern "(") (S.Replacement "-") $
        replaceAll (S.Pattern ")") (S.Replacement "-") $
        replaceAll (S.Pattern ".") (S.Replacement "-") $
        replaceAll (S.Pattern ",") (S.Replacement "-") $
        replaceAll (S.Pattern " ") (S.Replacement "-") $
        joinWith "-" $
          [ "elm-charts__pattern"
          , case design of
              Striped _ -> "striped"
              Dotted _ -> "dotted"
              Gradient _ -> "gradient"
          ] <> props

  toPatternDefs id spacing rotate inside =
        H.defs []
          [ H.pattern
              [ P.id id
              , P.patternUnits "userSpaceOnUse"
              , P.width spacing
              , P.height spacing
              , P.patternTransform $ "rotate(" <> show rotate <> ")"
              ]
              [ inside ]
          ]

  patternDefs /\ patternId =
        case design of
          Striped edits ->
            let
              config =
                  Helpers.applyFuncs edits
                    { color: defaultColor
                    , width: 3.0
                    , spacing: 4.0
                    , rotate: 45.0
                    }

              theId =
                  toPatternId
                    [ config.color
                    , show config.width
                    , show config.spacing
                    , show config.rotate
                    ]
            in
            (toPatternDefs theId config.spacing config.rotate $
                H.line
                  [ P.x1 0
                  , P.y 0
                  , P.x2 0
                  , P.y2 config.spacing
                  , P.stroke config.color
                  , P.strokeWidth config.width
                  ]
            ) /\ theId


          Dotted edits ->
            let
              config =
                  Helpers.applyFuncs edits
                    { color: defaultColor
                    , width: 3.0
                    , spacing: 4.0
                    , rotate: 45.0
                    }

              theId =
                  toPatternId
                    [ config.color
                    , show config.width
                    , show config.spacing
                    , show config.rotate
                    ]
            in
            (toPatternDefs theId config.spacing config.rotate $
                H.circle
                  [ P.fill config.color
                  , P.cx $ config.width / 3.0
                  , P.cy $ config.width / 3.0
                  , P.r $ config.width / 3.0
                  ]
            ) /\ theId

          Gradient edits ->
            let
              colors = if edits == [] then [ defaultColor, "white" ] else edits

              theId = toPatternId colors
              totalColors = length colors
              toPercentage i = Int.toNumber i / Int.toNumber (totalColors - 1)
              toStop i c =
                H.stop
                  [ P.offset $ pc (toPercentage i)
                  , P.stopColor c
                  ]
            in
            ( H.defs []
                [ H.linearGradient
                    [ P.id theId, P.x1 0, P.x2 0, P.y1 0, P.y2 1 ]
                    (mapWithIndex toStop colors)
                ]
            ) /\ theId

-- INTERVALS


newtype Generator a
  = Generator (Int -> Coord.Axis -> Array a)


numbers :: Generator Number
numbers = Generator $ \i b -> I.floats (I.around i) { min: b.min, max: b.max }


ints :: Generator Int
ints = Generator $ \i b -> I.ints (I.around i) { min: b.min, max: b.max }

{-
times : Time.Zone -> Generator I.Time
times zone =
  Generator (\i b -> I.times zone i { min = b.min, max = b.max })
-}

generate :: forall a. Int -> Generator a -> Coord.Axis -> Array a
generate amount (Generator func) limits = func amount limits




-- SYSTEM

fromSvg :: Plane -> Point -> Point
fromSvg plane point =
  { x: Coord.toCartesianX plane point.x
  , y: Coord.toCartesianY plane point.y
  }


fromCartesian :: Plane -> Point -> Point
fromCartesian plane point =
  { x: Coord.toSVGX plane point.x
  , y: Coord.toSVGY plane point.y
  }


lengthInSvgX :: Plane -> Number -> Number
lengthInSvgX = Coord.scaleSVGX


lengthInSvgY :: Plane -> Number -> Number
lengthInSvgY = Coord.scaleSVGY


lengthInCartesianX :: Plane -> Number -> Number
lengthInCartesianX = Coord.scaleCartesianX


lengthInCartesianY :: Plane -> Number -> Number
lengthInCartesianY = Coord.scaleCartesianY




-- HELPERS

withBorder :: forall a b. Array a -> (a -> a -> b) -> Maybe b
withBorder stuff func = do
  h <- head stuff
  t <- last stuff
  Just $ func h t

closestToZero :: Plane -> Number
closestToZero plane = clamp plane.y.min plane.y.max 0.0


isWithinPlane :: Plane -> Number -> Number -> Boolean
isWithinPlane plane x y =
  clamp plane.x.min plane.x.max x == x && clamp plane.y.min plane.y.max y == y


withAttrs :: forall msg
            . Array (H.Prop Void)
            -> (Array (H.Prop msg) -> Array (Html msg) -> Html msg)
            -> Array (H.Prop msg) 
            -> Array (Html msg) 
            -> Html msg
withAttrs attrs toEl defaultAttrs =
  toEl (defaultAttrs <> map (map absurd) attrs)

withAttrs' :: forall msg
            . Array (H.Prop Void)
            -> (Array (H.Prop msg) -> Html msg)
            -> Array (H.Prop msg)
            -> Html msg
withAttrs' attrs toEl defaultAttrs =
  toEl (defaultAttrs <> map (map absurd) attrs)

withinChartArea :: forall msg. Plane -> H.Prop msg
withinChartArea plane = P.clipPath $ "url(#" <> Coord.toId plane <> ")"
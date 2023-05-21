module Pha.Charts.Attributes where

import Prelude
import Data.Maybe (Maybe(..))
import Pha.Charts.Internal.Svg as CS

type Attribute c = c -> c

border :: forall r. String -> Attribute { border :: String | r }
border v = _ { border = v }

color :: forall r. String -> Attribute { color :: String | r }
color v config = if v == "" then config else config { color = v }

roundTop :: forall r. Number -> Attribute { roundTop :: Number | r }
roundTop v = _ { roundTop = v }

roundBottom :: forall r. Number -> Attribute { roundBottom :: Number | r }
roundBottom v = _ { roundBottom = v }

width :: forall r. Number -> Attribute { width :: Number | r }
width v = _ { width = v }

-- DECORATION

opacity :: forall r. Number -> Attribute { opacity :: Number | r }
opacity v = _ { opacity = v }


-- LINES

circle :: forall r. Attribute { shape :: Maybe CS.Shape | r }
circle  = _ { shape = Just CS.Circle }


triangle :: forall r. Attribute { shape :: Maybe CS.Shape | r }
triangle = _ { shape = Just CS.Triangle }

square :: forall r. Attribute { shape :: Maybe CS.Shape | r }
square = _ { shape = Just CS.Square }

diamond :: forall r. Attribute { shape :: Maybe CS.Shape | r }
diamond = _ { shape = Just CS.Diamond }


plus :: forall r. Attribute { shape :: Maybe CS.Shape | r }
plus = _ { shape = Just CS.Plus }

cross :: forall r. Attribute { shape :: Maybe CS.Shape | r }
cross = _ { shape = Just CS.Cross }


-- FOCAL

linear :: forall r. Attribute { method :: Maybe CS.Method | r }
linear = _ { method = Just CS.Linear }
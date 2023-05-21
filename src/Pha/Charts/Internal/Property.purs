module Pha.Charts.Internal.Property where

import Prelude
import Data.Array (foldl, reverse)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)

data Property data_ meta inter deco
  = Property (Config data_ meta inter deco)
  | Stacked (Array (Config data_ meta inter deco))

type Config data_ meta inter deco =
  { value :: data_ -> Maybe Number
  , visual :: data_ -> Maybe Number
  , format :: data_ -> String
  , meta :: Maybe meta
  , inter :: Array (inter -> inter)
  , attrs :: Array (deco -> deco)
  , extra :: Int -> Int -> Int -> Maybe meta -> data_ -> Array (deco -> deco)
  }


property :: forall data_ inter meta deco
            . (data_ -> Maybe Number) 
            -> Array (inter -> inter) 
            -> Array (deco -> deco) 
            -> Property data_ meta inter deco
property value inter attrs =
  Property
    { value: value
    , visual: value
    , format: value >>> map show >>> fromMaybe "N/A"
    , meta: Nothing
    , inter: inter
    , attrs: attrs
    , extra: \_ _ _ _ _ -> []
    }


format :: forall data_ meta inter deco
            . (Maybe Number -> String) 
            -> Property data_ meta inter deco 
            -> Property data_ meta inter deco
format value prop =
  case prop of
    Property con -> Property con { format = con.value >>> value }
    Stacked cons -> Stacked (map (\con -> con { format = con.value >>> value }) cons)

meta :: forall data_ meta inter deco
        . meta 
        -> Property data_ meta inter deco 
        -> Property data_ meta inter deco
meta value prop =
  case prop of
    Property con -> Property con { meta = Just value }
    Stacked cons -> Stacked (map (_ { meta = Just value }) cons)

variation :: forall data_ meta inter deco
            . (Int -> Int -> Int -> Maybe meta -> data_ -> Array (deco -> deco)) 
            -> Property data_ meta inter deco 
            -> Property data_ meta inter deco
variation attrs prop =
  case prop of
    Property c ->  Property c { extra = \p s i m d -> c.extra p s i m d <> attrs p s i m d }
    Stacked cs -> Stacked (map (\c -> c { extra = \p s i m d -> c.extra p s i m d <> attrs p s i m d }) cs)


-- todo
stacked :: forall data_ meta inter deco
            . Array (Property data_ meta inter deco) 
            -> Property data_ meta inter deco
stacked properties = Stacked (foldl stack {result: [], prev: []} configs).result
  where
  configs = reverse properties >>= toConfigs

  stack {prev, result} one =
    let toYs_ = [one.value] <> prev in
    {prev: toYs_, result: [one { visual = toVisual toYs_ }] <> result}

  toVisual toYs_ datum = do
    vs <- traverse (\toY -> toY datum) toYs_
    Just (sum vs)
  

toYs :: forall data_ meta inter deco
            . Array (Property data_ meta inter deco) 
            -> Array (data_ -> Maybe Number)
toYs properties = properties >>= each
  where
  each prop =
    case prop of
      Property config -> [ config.visual ]
      Stacked configs -> _.visual <$> configs

toConfigs :: forall data_ meta inter deco
                . Property data_ meta inter deco 
                -> Array (Config data_ meta inter deco)
toConfigs prop =
  case prop of
    Property config -> [ config ]
    Stacked configs -> configs
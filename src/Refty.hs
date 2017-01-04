{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Refty
( Key
, Identifier
, Entity
, Resource
, Reference
, Builder
, Refty
, resource
, selfRef
, hasOneRef
, hasManyRef
, builder
, refty
) where

import Data.Aeson (ToJSON, ToJSONKey, toJSON, (.=), Value, object)
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

type Key = T.Text
type Identifier a b = a -> b
type Entity a = Either a [a]

data Resource a b = Resource Key (Identifier a b) (Entity a)

data Reference a b = SelfRef Key
                   | HasOneRef Key (Identifier a b)
                   | HasManyRef Key (Identifier a b)

data Builder = forall a b. (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Builder [Reference a b] (Resource a b)

data Refty = Refty [Builder]

resource :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Key -> Identifier a b -> Entity a -> Resource a b
resource k i e = Resource k i e

selfRef :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Key -> Reference a b
selfRef k = SelfRef k

hasOneRef :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Key -> Identifier a b -> Reference a b
hasOneRef k i = HasOneRef k i

hasManyRef :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Key -> Identifier a b -> Reference a b
hasManyRef k i = HasManyRef k i

builder :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b)=> [Reference a b] -> Resource a b -> Builder
builder refs res = Builder refs res

refty :: [Builder] -> Refty
refty bs = Refty bs

reference :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Resource a b -> Reference a b -> (T.Text, Value)
reference (Resource _ i entity) (SelfRef k) =
  case entity of
    Left e -> k .= (i e)
    Right es -> k .= (map i es)

reference (Resource _ i entity) (HasOneRef k ki) = (k .=) $ M.fromList $
  case entity of
    Left e -> [(ki e, i e)]
    Right es -> map (\e -> (ki e, i e)) es

reference (Resource _ i entity) (HasManyRef k ki) = (k .=) $
  case entity of
    Left e ->
      M.fromList $ [(ki e, [i e])]
    Right es ->
      M.fromListWith (flip (++)) $ map (\e -> (ki e, [i e])) es

references :: Builder -> [(T.Text, Value)]
references (Builder refs res) = map (reference res) refs

entities :: Builder -> (T.Text, Value)
entities (Builder refs (Resource k i entity)) =
  case entity of
    Left e ->
      entities $ builder refs $ resource k i $ Right [e]
    Right es ->
      (k .=) $ M.fromList $ map (\e -> (i e, e)) es

instance ToJSON Refty where
  toJSON (Refty builders) = object
    [ "references" .= (object $ concat $ map references builders)
    , "entities" .= (object $ map entities builders)
    ]

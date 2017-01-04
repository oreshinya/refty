-- |
-- Module      : Refty
-- Copyright   : (c) 2017 Shinya Takahashi
-- License     : MIT
-- Maintainer  : Shinya Takahashi <s.takahashi313@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Reference + Entity = Refty ♡
--
-- Formatted JSON generator for API server inspired by normalizr.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Refty
  ( -- * Usage
    -- $usage

    -- * Data types
    Key
  , Identifier
  , Entity
  , Resource(..)
  , Reference(..)
  , Builder(..)
  , Refty(..)
    -- * Constructor functions
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

-- | Key of JSON.
type Key = T.Text

-- | Identifier getter of entity.
type Identifier a b = a -> b

-- | Single or multiple entity.
type Entity a = Either a [a]

-- | Basic entity information.
data Resource a b = Resource Key (Identifier a b) (Entity a)

-- | Reference information of entity.
data Reference a b = SelfRef Key
                   | HasOneRef Key (Identifier a b)
                   | HasManyRef Key (Identifier a b)

-- | Information builder related entity.
data Builder = forall a b. (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Builder [Reference a b] (Resource a b)

-- | JSON presenter.
data Refty = Refty [Builder]

-- | Constructor function for Resource.
resource :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Key -> Identifier a b -> Entity a -> Resource a b
resource k i e = Resource k i e

-- | Constructor function for Reference.
selfRef :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Key -> Reference a b
selfRef k = SelfRef k

-- | Constructor function for Reference.
hasOneRef :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Key -> Identifier a b -> Reference a b
hasOneRef k i = HasOneRef k i

-- | Constructor function for Reference.
hasManyRef :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Key -> Identifier a b -> Reference a b
hasManyRef k i = HasManyRef k i

-- | Constructor function for Builder.
builder :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b)=> [Reference a b] -> Resource a b -> Builder
builder refs res = Builder refs res

-- | Constructor function for Refty.
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


-- $usage
--
-- We will generate json like:
--
-- > {
-- >   "entities": {
-- >     "users": {
-- >       "1": { "id": 1, "name": "Lelouch" },
-- >       "2": { "id": 2, "name": "Suzaku" },
-- >       "3": { "id": 3, "name": "Jeremiah" }
-- >     },
-- >     "comments": {
-- >       "1": { "id": 1, "body": "Hello", "userId": 1 },
-- >       "4": { "id": 4, "body": "Foo", "userId": 1 },
-- >       "2": { "id": 2, "body": "World", "userId": 1 },
-- >       "3": { "id": 3, "body": ":)", "userId": 3 }
-- >     }
-- >   },
-- >   "references": {
-- >     "users": [ 2, 1, 3 ],
-- >     "userComments": { "1": [ 1, 4, 2 ], "3": [ 3 ] }
-- >   }
-- > }
--
-- First, define data types like:
--
-- User.hs:
--
-- @
-- import Data.Aeson
-- import Data.Text as T
-- import GHC.Generics (Generic)
--
-- data User = User
--   { id :: Int
--   , name :: T.Text
--   } deriving (Generic, Show)
--
-- instance ToJSON User
-- @
--
-- Comment.hs:
--
-- @
-- import Data.Aeson
-- import Data.Text as T
-- import GHC.Generics (Generic)
--
-- data Comment = Comment
--   { id :: Int
--   , userId :: Int
--   , body :: T.Text
--   } deriving (Generic, Show)
--
-- instance ToJSON Comment
-- @
--
-- Next, create JSON.
--
-- > import Data.Aeson
-- > import Refty
-- > import qualified User as U
-- > import qualified Comment as C
-- >
-- > -- Sample data
-- > users =
-- >   [ U.User 2 "Suzaku"
-- >   , U.User 1 "Lelouch"
-- >   , U.User 3 "Jeremiah"
-- >   ]
-- >
-- > comments =
-- >   [ C.Comment 1 1 "Hello"
-- >   , C.Comment 4 1 "Foo"
-- >   , C.Comment 2 1 "World"
-- >   , C.Comment 3 3 ":)"
-- >   ]
-- >
-- > encode $ refty
-- >   [ builder [ selfRef "users" ] $ resource "users" U.id $ Right users
-- >   , builder [ hasManyRef "userComments" C.userId ] $ resource "comments" C.id $ Right comments
-- >   ]

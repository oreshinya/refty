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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Refty
  ( -- * Usage
    -- $usage

    -- * Data types
    Key
  , Identifier
  , Entity(..)
  , Resource(..)
  , Reference(..)
  , Builder(..)
  , Refty(..)

    -- * Constructor functions
  , singleEntity
  , listEntity
  , resource
  , selfRef
  , hasOneRef
  , hasManyRef
  , builder
  , refty
  ) where

import           Control.Arrow ((&&&))
import           Data.Aeson    (ToJSON, ToJSONKey, Value, object, toJSON, (.=))
import qualified Data.Map.Lazy as M
import qualified Data.Text     as T

-- | Key of JSON.
type Key = T.Text

-- | Identifier getter of entity.
type Identifier a b = a -> b

-- | Single or multiple entity.
data Entity a = SingleEntity a | ListEntity [a]

-- | Basic entity information.
data Resource a b = Resource Key (Identifier a b) (Entity a)

-- | Reference information of entity.
data Reference a b = SelfRef Key
                   | HasOneRef Key (Identifier a b)
                   | HasManyRef Key (Identifier a b)

-- | Information builder related entity.
data Builder = forall a b. (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Builder (Resource a b) [Reference a b]

-- | JSON presenter.
newtype Refty = Refty [Builder]

-- | Constructor function for Entity.
singleEntity :: (ToJSON a) => a -> Entity a
singleEntity = SingleEntity

-- | Constructor function for Entity.
listEntity :: (ToJSON a) => [a] -> Entity a
listEntity = ListEntity

-- | Constructor function for Resource.
resource :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Key -> Identifier a b -> Entity a -> Resource a b
resource = Resource

-- | Constructor function for Reference.
selfRef :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Key -> Reference a b
selfRef = SelfRef

-- | Constructor function for Reference.
hasOneRef :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Key -> Identifier a b -> Reference a b
hasOneRef = HasOneRef

-- | Constructor function for Reference.
hasManyRef :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Key -> Identifier a b -> Reference a b
hasManyRef = HasManyRef

-- | Constructor function for Builder.
builder :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Resource a b -> [Reference a b] -> Builder
builder = Builder

-- | Constructor function for Refty.
refty :: [Builder] -> Refty
refty = Refty

reference :: (ToJSON a, ToJSON b, ToJSONKey b, Ord b) => Resource a b -> Reference a b -> (T.Text, Value)
reference (Resource _ i entity) (SelfRef k) =
  case entity of
    SingleEntity e -> k .= i e
    ListEntity es  -> k .= map i es

reference (Resource rk i entity) ref@(HasOneRef k ki) =
  case entity of
    SingleEntity e -> reference (resource rk i $ ListEntity [e]) ref
    ListEntity es  -> (k .=) $ M.fromList $ map (ki &&& i) es

reference (Resource rk i entity) ref@(HasManyRef k ki) =
  case entity of
    SingleEntity e   -> reference (resource rk i $ ListEntity [e]) ref
    ListEntity es -> (k .=) $ M.fromListWith (flip (++)) $ map (\e -> (ki e, [i e])) es

references :: Builder -> [(T.Text, Value)]
references (Builder res refs) = map (reference res) refs

entities :: Builder -> (T.Text, Value)
entities (Builder (Resource k i entity) refs) =
  case entity of
    SingleEntity e ->
      entities $ builder (resource k i $ ListEntity [e]) refs
    ListEntity es ->
      (k .=) $ M.fromList $ map (\e -> (i e, e)) es

instance ToJSON Refty where
  toJSON (Refty builders) = object
    [ "references" .= object (concatMap references builders)
    , "entities" .= object (map entities builders)
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
-- >     "self": [ 2, 1, 3 ],
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
-- >   [ builder (resource "users" U.id $ ListEntity users) [ selfRef "self" ]
-- >   , builder (resource "comments" C.id $ ListEntity comments) [ hasManyRef "userComments" C.userId ]
-- >   ]

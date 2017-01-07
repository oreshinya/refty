{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.Text    as T
import           GHC.Generics (Generic)
import           Refty

data User = User { idOfUser :: Int, name :: T.Text } deriving (Generic, Show)
data Comment = Comment { idOfComment :: Int, userId :: Int, body :: T.Text } deriving (Generic, Show)
data Item = Item { idOfItem :: Int, commentId :: Int } deriving (Generic, Show)

instance ToJSON User
instance ToJSON Comment
instance ToJSON Item

users = [ User 2 "Ryoma"
        , User 1 "Shinya"
        , User 3 "Koji"
        ]

comments = [ Comment 1 1 "Hello"
           , Comment 4 1 "Foo"
           , Comment 2 1 "World"
           , Comment 3 3 ":)"
           ]

items = [ Item 1 1
        , Item 2 3
        , Item 3 2
        ]

t = encode $ refty [ builder (resource "users" idOfUser $ Right users)
                       [selfRef "users"]
                   , builder (resource "comments" idOfComment $ Right comments)
                       [hasManyRef "userComments" userId]
                   , builder (resource "items" idOfItem $ Right items)
                       [hasOneRef "commentItems" commentId]
                   ]

main :: IO ()
main = print t

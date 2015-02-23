{-# OPTIONS_GHC -Wall #-}
module Model where

import Data.Set as Set

-- http://json.org/

type Key = String

data Json = JObject Key (Set JValue)
            | JArray JArr
            deriving Show

data JObj = JObj Key JValue
            deriving (Show, Eq, Ord)

data JArr = Arr [JValue] deriving (Show, Eq, Ord)

data Null = Null deriving (Show, Eq, Ord)

data JValue = Num Double
              | S String
              | B Bool
              | J JObj
              | Array JArr
              | N Null
               deriving (Show, Eq, Ord)
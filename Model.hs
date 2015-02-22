{-# OPTIONS_GHC -Wall #-}
module Model where

import Data.Set 

-- http://json.org/

type Key = String

data Json = JObject (Set JObj)
            | JArray JArr

data JObj = JObj Key JValue
            deriving Show

data JArr = Arr [JValue] deriving Show

data Null = Null deriving Show

data JValue = Num Double
              | S String
              | B Bool
              | J JObj
              | Array JArr
              | N Null
               deriving Show
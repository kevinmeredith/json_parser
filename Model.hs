{-# OPTIONS_GHC -Wall #-}
module Model where

type Key = String

data Json = JObj Key JValue 
            | Arr [JValue] 
            deriving Show

data JValue = N Double
              | S String
              | B Bool
              | J Json
               deriving Show
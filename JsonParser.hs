{-# OPTIONS_GHC -Wall #-}
module JsonParser where

import SExpr
import Model
import AParser
import Control.Applicative

parseQuote :: Parser Char
parseQuote = satisfy (== '"')

parseNotQuote :: Parser Char
parseNotQuote = satisfy (/= '"')

parseStringJValue :: Parser JValue
parseStringJValue = (\x -> S x) <$> ((zeroOrMore spaces) *> parseQuote *> (zeroOrMore spaces) *> (zeroOrMore parseNotQuote))
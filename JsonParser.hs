{-# OPTIONS_GHC -Wall #-}
module JsonParser where

import SExpr hiding (N)
import Model
import AParser
import Control.Applicative
import Data.Char

parseStringJValue :: Parser JValue
parseStringJValue = (\x -> S x) <$> zeroOrMore (satisfy (const True))

--parseNumberJValue :: Parser JValue
--parseNumberJValue = alt parseUnsignedNumber

parsePositiveInteger :: Parser JValue
parsePositiveInteger = fmap (\x -> N (fromIntegral x)) parseInteger 

parseNegativeInteger :: Parser JValue
parseNegativeInteger = fmap (\x -> N (fromIntegral (-x))) $ (parseNegativeSign *> parseInteger)

-- examples of decimal points
-- 0.
-- .5
-- 1.0
-- -2.0

-- 1.
parsePositiveDecimal:: Parser JValue
parsePositiveDecimal = (\x _ y -> f x y) <$> ( (oneOrMore (satisfy isNumber)) <*> parseDecimalPoint <*> (zeroOrMore (satisfy isNumber)) )
  where 
  	f x [] = N (read x)
  	f x y  = N (-(readWholeAndDecimal (read x) (read y)))

--parseNegativeDecimal :: Parser JValue
--parseNegativeDecimal = f <$> (parseNegativeSign *> (oneOrMore (satisfy isNumber)) <* parseDecimalPoint *> (zeroOrMore (satisfy isNumber)) )
--  where 
--  	f x [] = N (read (-x))
--  	f x y  = N (-(readWholeAndDecimal x y))

--parsePositiveDecimalOnly :: Parser JValue
--parsePositiveDecimalOnly = N <$> (parseDecimalPoint *> parseInteger)

parseNegativeSign :: Parser Char
parseNegativeSign = satisfy (== '-')

parseDecimalPoint :: Parser Char
parseDecimalPoint = satisfy (== '.')

parseInteger :: Parser Integer
parseInteger = fmap read $ (oneOrMore (satisfy isNumber))

-- TODO: overflow with `Integer`?
type Whole   = Integer
type Decimal = Integer

readWholeAndDecimal :: Whole -> Decimal -> Double
readWholeAndDecimal w d = read $ (show w) ++ "." ++ (show d)



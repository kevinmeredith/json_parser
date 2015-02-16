{-# OPTIONS_GHC -Wall #-}
module JsonParser where

import SExpr hiding (N)
import Model
import AParser
import Control.Applicative
import Data.Char

parseJValue :: Parser JValue
parseJValue = alt (alt parseBooleanJValue parseNumberJValue) parseStringJValue 

parseArrayJValue :: Parser Json
parseArrayJValue = fmap Arr (zeroOrMore $ spaces *> parseJValue <* spaces <* (satisfy (== ',')))

parseBooleanJValue :: Parser JValue
parseBooleanJValue = fmap B parseBool

parseStringJValue :: Parser JValue
parseStringJValue = S <$> ((char '"') *> (zeroOrMore notEndOfString) <* (char '"'))

parseNumberJValue :: Parser JValue
parseNumberJValue = alt (alt parsePositiveDecimal parseNegativeDecimal) (alt parsePositiveInteger parseNegativeInteger)

parsePositiveInteger :: Parser JValue
parsePositiveInteger = fmap (\x -> N (fromIntegral x)) parseInteger 

parseNegativeInteger :: Parser JValue
parseNegativeInteger = fmap (\x -> N (fromIntegral (-x))) $ (parseNegativeSign *> parseInteger)

-- TODO: implement `not`

-- examples of decimal points
-- 0.
-- .5
-- 1.0
-- -2.0

parsePositiveDecimal:: Parser JValue
parsePositiveDecimal = (\x _ y -> f x y) <$> (oneOrMore (satisfy isNumber)) <*> parseDecimalPoint <*> (zeroOrMore (satisfy isNumber)) 
  where
    f x [] = N (read x)
    f x y  = N ((readWholeAndDecimal (read x) (read y)))

parseNegativeDecimal:: Parser JValue
parseNegativeDecimal = (\_ x _ y -> f x y) <$> parseNegativeSign <*> (oneOrMore (satisfy isNumber)) <*> parseDecimalPoint <*> (zeroOrMore (satisfy isNumber))
  where
    f x [] = N (read x)
    f x y  = N (-(readWholeAndDecimal (read x) (read y)))

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

data EndOfString = EndOfString deriving Show

parseBool :: Parser Bool
parseBool = Parser f
  where
    f ('t':'r':'u':'e':xs) 	   = Just (True, xs)
    f ('f':'a':'l':'s':'e':xs) = Just (False, xs)
    f _                        = Nothing

--" \"foo\":\"bar\" "

---- assumes that an opening quote was already parsed
--parseStartsWithQuotes :: Parser String
--parseStartsWithQuotes = Parser f
--  where
--    f ('\"':xs)  | all isSpace xs = Just ([], [])
--    f xs         | all isSpace xs = Nothing
--    f (x:xs)                      = Just (x, xs)

-- originally I thought it was needed for -1234.3.3, but perhaps the remaining input can be parsed afterwards
endOfString :: Parser EndOfString
endOfString = Parser f
  where
    f [] = Just (EndOfString, [])
    f _  = Nothing

notEndOfString :: Parser Char
notEndOfString = Parser f
  where 
    f []     = Nothing
    f (x:xs) = Just (x, xs)


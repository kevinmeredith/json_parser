{-# OPTIONS_GHC -Wall #-}
module JsonParser where

import SExpr hiding (N)
import Model
import AParser
import Control.Applicative
import Data.Char
import Data.Set as Set

parseJson :: Parser Json
parseJson = alt (jObjToJson <$> parseJObj) (JArray <$> parseArray)

parseJObj :: Parser JObj 
parseJObj = (\k v -> JObj k v) <$> 
                      (spaces *> (char '{') *> spaces *> parseJObjKey <* spaces <* (char ':')) <*> 
                      (spaces *> parseJValue <* spaces <* (char '}'))

jObjToJson :: JObj -> Json
jObjToJson (JObj k v) = JObject k (convertArrToSet v)

parseJObjKey :: Parser Key
parseJObjKey = (char '"') *> oneOrMore (notChar '"') <* char ('"')

parseJValue :: Parser JValue
parseJValue = alt 
                   (alt parseBooleanJValue parseNumberJValue) 
                   (alt parseNull (alt parseArrayJValue parseStringJValue))

parseArrayJValue :: Parser JValue
parseArrayJValue = fmap (Array) parseArray

parseArray :: Parser JArr
parseArray = fmap Arr $ 
                 (char '[' *> spaces *> (alt (rep1sep parseJValue (char ',')) ((: []) <$> parseSingleJValue)) <* spaces <* char ']')

parseBooleanJValue :: Parser JValue
parseBooleanJValue = fmap B parseBool

parseStringJValue :: Parser JValue
parseStringJValue = S <$> ((char '"') *> (zeroOrMore (alt (char '\\' *> char '"') (notChar '"'))) <* (char '"'))

parseNumberJValue :: Parser JValue
parseNumberJValue = alt (alt parsePositiveDecimal parseNegativeDecimal) (alt parsePositiveInteger parseNegativeInteger)

parseNull :: Parser JValue
parseNull = (\_ -> N Null) <$> (char 'n' *> char 'u' *> char 'l' *> char 'l')

parsePositiveInteger :: Parser JValue
parsePositiveInteger = fmap (\x -> Num (fromIntegral x)) parseInteger 

parseNegativeInteger :: Parser JValue
parseNegativeInteger = fmap (\x -> Num (fromIntegral (-x))) $ (parseNegativeSign *> parseInteger)

parseSingleJValue :: Parser JValue
parseSingleJValue = alt 
                     (alt parseBooleanJValue parseNumberJValue) 
                     (alt parseNull parseStringJValue)

-- 0 or more repetitions
repsep :: Parser a -> Parser b -> Parser [a]
repsep p sep = alt (rep1sep p sep) ((: []) <$> p)

-- 1 or more repetitions
rep1sep :: Parser a -> Parser b -> Parser [a]
rep1sep p sep = (\xs y -> xs ++ [y]) <$> (oneOrMore (p <* spaces <* sep <* spaces)) <*> p

-- examples of decimal points
-- 0.
-- .5
-- 1.0
-- -2.0

parsePositiveDecimal:: Parser JValue
parsePositiveDecimal = (\x _ y -> f x y) <$> (oneOrMore (satisfy isNumber)) <*> parseDecimalPoint <*> (zeroOrMore (satisfy isNumber)) 
  where
    f x [] = Num (read x)
    f x y  = Num ((readWholeAndDecimal (read x) (read y)))

parseNegativeDecimal:: Parser JValue
parseNegativeDecimal = (\_ x _ y -> f x y) <$> parseNegativeSign <*> (oneOrMore (satisfy isNumber)) <*> parseDecimalPoint <*> (zeroOrMore (satisfy isNumber))
  where
    f x [] = Num (read x)
    f x y  = Num (-(readWholeAndDecimal (read x) (read y)))

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

convertArrToSet :: JValue -> (Set JValue)
convertArrToSet (Array (Arr xs))  = Prelude.foldr (\x acc -> Set.insert x acc) Set.empty xs
convertArrToSet x                 = Set.singleton x

parseBool :: Parser Bool
parseBool = Parser f
  where
    f ('t':'r':'u':'e':xs) 	   = Just (True, xs)
    f ('f':'a':'l':'s':'e':xs) = Just (False, xs)
    f _                        = Nothing

notChar :: Char -> Parser Char
notChar c = satisfy (/= c)

{-# OPTIONS_GHC -Wall #-}
module JsonParser where

import SExpr hiding (N)
import Model
import AParser
import Control.Applicative
import Data.Char

parseQuote :: Parser Char
parseQuote = satisfy (== '"')

parseNotQuote :: Parser Char
parseNotQuote = satisfy (/= '"')

-- TODO: handle escaped \" - " foo \" bar BIZ "
--parseStringJValue :: Parser JValue
--parseStringJValue = (\x -> S x) <$> (parseQuote *> spaces *> (zeroOrMore parseNotQuote) <* parseQuote)

--parseNumberJValue :: Parser JValue
--parseNumberJValue = alt parseUnsignedNumber

parsePositiveInteger :: Parser JValue
parsePositiveInteger = fmap (\x -> N (fromIntegral x)) parseInteger 

parseNegativeInteger :: Parser JValue
parseNegativeInteger = fmap (\x -> N (fromIntegral (-x))) $ (parseNegativeSign *> parseInteger)

--parseUnsignedDecimal:: Parser JValue
--parseUnsignedDecimal = (\x y -> N (read x)) <$> ( (oneOrMore (satisfy isNumber)) <* parseDecimalPoint *> (zeroOrMore (satisfy isNumber)) )

--parseSignedDecimal :: Parser JValue
--parseSignedDecimal = (\x y -> N (read (-x)))) <$> (parseNegativeSign *> (oneOrMore (satisfy isNumber)) <* parseDecimalPoint *> (zeroOrMore (satisfy isNumber)) )

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



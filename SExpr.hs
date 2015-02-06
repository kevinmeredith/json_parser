-- http://www.cis.upenn.edu/~cis194/spring13/hw/10-applicative.pdf
{-# OPTIONS_GHC -Wall #-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = alt ( (:) <$> p <*> zeroOrMore p) (pure [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> (zeroOrMore (satisfy isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show, Eq)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Show, Eq)

parseAtom :: Parser Atom
parseAtom = alt n i
   where n = (\z -> N z) <$> posInt
         i = (\z -> I z) <$> ident

parseAAtom :: Parser SExpr
parseAAtom = fmap (\x -> A x) parseAtom         

-- see http://stackoverflow.com/questions/27894888/parsing-s-expression for its Haskell representation

parseComb :: Parser SExpr
parseComb = (\x -> Comb x) <$> ( spaces *> (char '(') *> (oneOrMore parseSExpr) <* (char ')') <* spaces )

parseSExpr :: Parser SExpr
parseSExpr = alt (spaces *> parseAAtom <* spaces) parseComb

-- testing per HW samples

test :: Bool
test = all (== True) [testAtomN, testAtomI, testComb1, testComb2, testComb3]

testAtomN :: Bool
testAtomN = case (runParser parseSExpr "5") of Just (x, _) -> (x == A (N 5))
                                               Nothing     -> False

testAtomI :: Bool
testAtomI = case (runParser parseSExpr "foo3") of Just (x, _) -> (x == A (I "foo3"))
                                                  Nothing     -> False

testComb1 :: Bool
testComb1 = case (runParser parseSExpr "(bar (foo) 3 5 874)") of Just (x, _) -> (x == expectedComb1)
                                                                 Nothing     -> False

expectedComb1 :: SExpr
expectedComb1 = Comb [A (I "bar"), Comb[A (I "foo")], A (N 3), A (N 5), A (N 874)]

testComb2 :: Bool
testComb2 = case (runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)") of Just (x, _) -> (x == expectedComb2)
                                                                                      Nothing     -> False

expectedComb2 :: SExpr
expectedComb2 = Comb [ Comb [ Comb [A (I "lambda"), A (I "x"), Comb [A (I "lambda"), A (I "y"), Comb [A (I "plus"), A (I "x"), A (I "y")]]], A (N 3)], A (N 5)]

testComb3 :: Bool
testComb3 = case (runParser parseSExpr "( lots of ( spaces in ) this ( one ) )") of Just (x, _) -> (x == expectedComb3)
                                                                                    Nothing     -> False

expectedComb3 :: SExpr
expectedComb3 = Comb [ A (I "lots"), A (I "of"), Comb [A (I "spaces"), A (I "in")], A (I "this"), Comb [ A (I "one")]]
-- http://www.cis.upenn.edu/~cis194/spring13/hw/10-applicative.pdf
-- copy from ../Homework10/AParser 
{-# OPTIONS_GHC -Wall #-}
module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parse--r for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1: Implement Functor for Parser
-- credit to `http://stackoverflow.com/a/27673488/409976` for help in answering

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c) 

instance Functor (Parser) where
  fmap g (Parser f)  = Parser $ fmap (first g) . f

-- pure a represents the parser which consumes no input and successfully
-- returns a result of a

--p1 <*> p2 represents the parser which first runs p1 (which will
--consume some input and produce a function), then passes the
--remaining input to p2 (which consumes more input and produces
--some value), then returns the result of applying the function to the
--value

-- credit to http://stackoverflow.com/questions/27698489/implement-applicative-parsers-apply-function/27698545#27698545
-- for helping me out!

foo :: Parser (Char -> Char)
foo = Parser f
  where 
    f []     = Nothing
    f (_:xs) = Just (const 'K', xs) 

instance Applicative (Parser) where
  pure x                    = Parser $ \xs -> Just (x, xs)
  (Parser f) <*> (Parser g) = Parser $ \xs -> case (f xs) of Nothing         -> Nothing
                                                             Just (fn, as)   -> case (g as) of Nothing      -> Nothing
                                                                                               Just (m, bs) -> Just (fn m, bs) 

-- Exercise 3: Implement Parsers using `apply`

pair :: a -> a -> (a, a)
pair x y = (x , y)

-- 'a' followed by 'b' parser
abParser :: Parser (Char, Char)
abParser = (fmap pair (satisfy (== 'a'))) <*> (satisfy (== 'b'))

-- same as `abParser`, but different return type
abParser_ :: Parser (())
abParser_ = (fmap (\_ _ -> ()) (satisfy (== 'a'))) <*> (satisfy (== 'b'))

singleSpace :: Parser Char 
singleSpace = Parser f
  where
    f []      = Nothing
    f (x:xs) 
     | isSpace x = Just (x, xs)
     | otherwise = Nothing 

-- runParser intPair "12 34" === Just([12, 34], [])
intPair :: Parser [Integer]
intPair = (fmap (\x _ y -> x : y : []) posInt) <*> singleSpace <*> posInt

-- Exercise 4.

class Applicative f => MyAlternative f where
  e     :: f a
  alt   :: f a -> f a -> f a 

instance MyAlternative Parser where
  e                          = Parser $ \_ -> Nothing
  alt (Parser f)  (Parser g) = Parser $ \xs -> case (f xs) of Nothing      -> g xs
                                                              Just (y, ys) -> Just (y, ys)

intOrUppercase :: Parser ()
intOrUppercase = alt (fmap (\_ -> ()) posInt) (fmap (\_ -> ()) (satisfy isUpper)) 
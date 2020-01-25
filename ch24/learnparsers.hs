module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators as TPC

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one :: Parser Char
one = char '1'

-- read a single character '1', then die
one' :: Parser Char
one' = one >> stop

-- read two characters, '1' and '2'
oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

-- read two characerts, '1' and '2', then die
oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = 
  print $ parseString p mempty "123"
  
testParse2 :: Show a => Parser a -> IO ()
testParse2 p = 
  print $ parseString p mempty "1"
{- 
The p argument is a parsers, specifically a character parser.
The functions and and oneTwo have the type Parser Char.
We needed to declare tye type of testParse in order to show what we 
  parsed because of ambiguity
The key thing to realise here is that we're using parser like values
  and combining them using the same stuff we use with ordinary functions
  or operators from the applicative and monad typeclasses
The "structure" that makes up the Applicative or Monad in this
  case is the Parser itself
-}

{-
Next we'll write a function to print a string to standard output
with a newline prefixed and then use that function as part
of a main function that will show us what we've got sofar
-}

pNL :: [Char] -> IO ()
pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  
  
-- Parsing Practice Exercise 1
one'' = one >> TPC.eof

one''' = one <* TPC.eof
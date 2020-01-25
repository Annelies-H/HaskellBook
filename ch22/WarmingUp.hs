module WarmingUp where

import Data.Char
import Control.Applicative

cap :: [Char] -> [Char]
cap xs = map toUpper xs

-- rev :: ((->) [Char]) [Char] <- de monad is (->) [Char]
rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap.rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char],[Char])
tupled xs = (cap xs, rev xs)

doTupled :: [Char] -> ([Char],[Char])
doTupled = do
  capped <- cap
  reversed <- rev
  return (capped, reversed)

bindTupled :: [Char] -> ([Char],[Char])
bindTupled =  
  cap >>= (\capped -> rev >>= 
  (\reversed -> return (capped, reversed)))

-- de monad is: functies die [Char] als input nemen  
-- in de type signature: ((->) [Char])
tupled' :: ((->) [Char])([Char],[Char])
tupled' = (,) <$> cap <*> rev

-- alleen de input hoeft hetzelfde te zijn voor de monad
-- je kan dus ook a -> b functies gebruiken
foo :: [Char] -> Bool
foo = even.length

-- fooTupled :: m a
-- m = 'string naar..' dus je kan er nog iets instoppen
-- als je er een argument in stopt blijft de a over
-- omdat de m de functie is
fooTupled :: ((->) [Char]) (Bool,[Char])
fooTupled = (,) <$> foo <*> rev
--eigenlijk is de type signature hierboven gewoon:
--fooTupled :: [Char] -> (Bool, [Char])

fooTupled' :: [Char] -> (Bool,[Char])
fooTupled' = liftA2 (,) foo rev
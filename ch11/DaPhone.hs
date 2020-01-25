module DaPhone where

import Data.Char
import Data.List

--
--create a data structure that captures the phone layout
data DaPhone = DaPhone [(Char, String)] deriving Show

  --I've kept the exact layout out of the data structure to allow different type of phone layouts
phone :: DaPhone
phone = DaPhone [('1', " 1")  , ('2',"abc2") , ('3',"def3") , 
                 ('4',"ghi4") , ('5',"jkl5") , ('6',"mno6") ,
                 ('7',"pqrs7"), ('8',"tuv8") , ('9',"wxyz9"), 
                 ('*',"^*")   , ('0',"+0")   , ('#',".,!?#")  ]

--
--convert the following conversation into the keypresses required to express them
convo :: [String]
convo = 
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol, ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "HAha thanks just making sure rofl ur turn"]
   
  -- validButtons = "123456789*#"
type Digit = Char
  -- valid presses: 1 and up
type Presses = Int


  --assuming the default phone definition
  -- 'a' -> [('2', 1)]
  -- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone []) _ = []
reverseTaps (DaPhone ((digit,text):xs)) a
    | elem (toLower a) text  = convert digit text a 1
    | otherwise = reverseTaps (DaPhone xs) a
  where 
     convert digit (x:xs) a presses  
         | toLower a == x = display a (digit, presses) 
         | otherwise = convert digit xs a (presses + 1)
     display a taps 
         | isUpper a = [('*', 1), taps]
         | otherwise = [taps]   
         
  --applying reverseTaps to a single text message      
reverseSMS :: DaPhone -> String -> [(Digit, Presses)]
reverseSMS aPhone []     = []
reverseSMS aPhone (x:xs) = reverseTaps aPhone x ++ reverseSMS aPhone xs
  
  -- converting the messages to the keys (digits) and number of times they need to be pressed
convertMessage :: [[(Digit, Presses)]]
convertMessage = map (reverseSMS phone) convo

--
-- How many times do digits need to be pressed for each message?

  --converts (a list of) digits and presses to the total number of presses
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps [] = 0
fingerTaps ((_,presses):xs) = presses + fingerTaps xs
  
  --returns a list with the total number of presses per message
totalTaps :: [Presses]
totalTaps = map fingerTaps convertMessage

--
-- What was the most popular letter for each message? What was it costs?
mostPopularLetter :: String -> Char
mostPopularLetter string = go groupedlength 0 'q'
   where
      nospace [] = ""
      nospace (' ':xs) = nospace xs
      nospace (x:xs) = x : nospace xs
      grouped = group (sort (nospace string)) :: [String]
      groupedlength = map (\x -> (head x, length x)) grouped  :: [(Char, Int)]
      go [] _ result = result
      go ((char,n):xs) max result
        | n > max = go xs n char
        | max >= n = go xs max result
        
mostPopLet :: [Char]
mostPopLet = map mostPopularLetter convo

mostPopLetCost :: [(Char,Presses)]
mostPopLetCost = map (\x -> (x,(fingerTaps(reverseTaps phone x)))) mostPopLet

--
-- What was the most popular letter overall? What was the most popular word?
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter.concat

mostPopularWord :: String -> String
mostPopularWord string = go groupedlength 0 ""
   where
      lowerstring = map toLower string
      grouped = group (sort (words lowerstring)) :: [[String]]
      groupedlength = map (\x -> (head x, length x)) grouped  :: [(String, Int)]
      go [] _ result = result
      go ((char,n):xs) max result
        | n > max = go xs n char
        | max >= n = go xs max result
        
coolestWord :: [String] -> String
coolestWord (x:xs) = mostPopularWord (strings (x:xs))
   where 
      strings [] = ""
      strings (x:xs) = x ++ [' '] ++ strings xs

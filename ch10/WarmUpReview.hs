stops = "pbtdkg"
vowels = "aeiou"

-- 1a

twotup :: [a] -> [b] -> [(a,b)]
twotup _ [] = []
twotup xs (y:ys) = tupx xs y ++ twotup xs ys
       where
         tupx [] _ = []
         tupx (x:xs) y = (x,y) : tupx xs y

combineTwo :: [(Char, Char)]         
combineTwo = twotup stops vowels

threetup :: [(a,b)] -> [c] -> [(a,b,c)]       
threetup [] _ = []
threetup _ [] = []
threetup ((x,y):xys) (z:zs) = tupxy ((x,y):xys) z ++ threetup ((x,y):xys) zs   
      where
         tupxy [] _ = []
         tupxy ((x,y):xys) z = (x,y,z) : tupxy xys z         

combineThree :: [(Char, Char, Char)]       
combineThree = threetup combineTwo stops    

-- 1b

onlyP :: [(Char, Char, Char)]
onlyP = filter f combineThree
  where
    f = (\(x,y,z) -> x == 'p')
    
-- 1c

nouns = ["pony", "chair", "painting", "car", "plant"]
verbs = ["eats", "looks", "sits", "drives"]

twoWords = twotup nouns verbs
threeWords = threetup twoWords nouns

-- 2

seekritFunc x = div (sum (map length (words x))) (length (words x))

{-
words x takes a a sentence (a string) and makes a list with all the separate words: words :: String -> [String]
(length (words x)) thus gives the number of words in a sentence (aka the length of the list of separate words)
(map length (words x)) maps lenght over the list of separate words and returns a list with the length of the words
(sum (map length (words x))) sums the list with the length of words returning the length of all words together
-> dividing the total length of the words by the number of words returns the average length of the words
-}

-- 3

seekritFrac x = (fromIntegral total) / (fromIntegral number)
   where 
      total = sum (map length (words x))
      number = length (words x)

    

--exercise 1
multi3 = filter (\x -> rem x 3 == 0) [1..30]

--exercise 2
howmany3 = length multi3
howmany3' = length (filter (\x -> rem x 3 == 0) [1..30])

--exercise 3

--using filter
myFilter :: String -> [String]
myFilter xs =  filter (\x -> x /= "the" && x /= "a" && x/= "an" && x /= "The" && x/= "A" && x /= "An" && x /= "THE" && x /= "AN") (words xs)

--using list comprehension
myFilter2 :: String -> [String]
myFilter2 xs = [x | x <- words xs, x /= "the", x /= "a", x /= "an"]

--specifiying a reusable list of words
wordlist :: [String]
wordlist = ["the", "a", "an"]

--notElem is the opposite of elem
myFilter3 :: String -> [String]
myFilter3 xs = filter (\x -> notElem x wordlist) (words xs)

--pointfree
myFilter4 = filter (\x -> notElem x wordlist).words

--making a string again
myFilter5 xs = unwords  $ myFilter4 xs

myFilter6 = unwords.myFilter
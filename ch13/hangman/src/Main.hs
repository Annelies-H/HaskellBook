module Main where

import Control.Monad (forever)
import Data.Char (toLower, toUpper)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- allWords reads the file with words and returns a list with separate words
-- the function lines splits the string at the newline mark (each new line becomes a new list entry)
newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList(lines dict)
  
-- gameWords creates a shorter list of words to be used in the game by filtering for all words
-- that are larger than the minimum length but shorter than the maximum length
minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w = 
            let l = length (w :: String)
            in l > minWordLength && l < maxWordLength
            
-- randomWord pulls a random Word out of a list of words
randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

-- randomWord' binds the gameWords list to the randomWord function  
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- the Puzzle datatype includes the word we're trying to guess, a list of characters correctly guessed so far 
-- and a list of the characters guess so far
data Puzzle = Puzzle String [Maybe Char] [Char]

-- our own show instance makes sure that it does not show the word that needs to be guessed
-- it shows only the correctly guessed characters (aka discovered) and our other guesses (guessed)
instance Show Puzzle where
  show (Puzzle _ discovered guessed) = 
    (intersperse ' ' $ fmap renderPuzzleChar discovered) 
     ++ ". So far you have also guessed:  " ++ guessed

-- renderPuzzleChar changes all undiscovered characters (the Nothing's) into an underscore 
-- and shows the correctly guessed characters (Just a) in their correpsonding place
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just a) = a
     
-- a function to create a new puzzle where all the characters of the new word are changed to Nothing (aka not guessed)
-- and nothing is guessed yet, so the list of guesses is empty     
freshPuzzle :: String -> Puzzle
freshPuzzle wrd = Puzzle wrd (map (const Nothing) wrd) []

-- a function to check whether a guessed charater is in the puzzle word
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle xs _ _) x = elem x xs

-- a function to check whether the guessed character has been checked before
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ xs) x = elem x xs

-- this function allows us to updates a puzzle based on a guessed character
-- the function checks whether it is a correctly guessed character or not, and appends it to the list of guessed characters
-- zipper is a combining function for deciding how to handle the character in the word, 
 -- what's been guessd already and the character that was just guessed
 -- it is used to zip the puzzleword (word) and the filledInSoFar list
 -- if the guessed character equals one in the word it is substituted for 'Just a' in the filledInSoFar list
 -- otherwise the filledInSoFar list stays the same 
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  if elem c word 
   then Puzzle word newFilledInSoFar (s) 
   else Puzzle word newFilledInSoFar (c:s)
  where
    zipper guessed wordChar guessChar = if wordChar == guessed
                                        then Just wordChar
                                        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

-- this function determines how a guess is handled using three options
-- 1. the character has been guessed before (alreadyGuessd returns True), it returns the same puzzle  
-- 2. the character was in the word (charInWord returns True), it returns the puzzle with the new character in the word
-- 3. the character isn't in the word (charInWord returns False), it returnes the puzzle with the char added to the guess list  
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"       
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly."
      return (fillInCharacter puzzle guess)
    (False,_) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

-- function to end the game after 7 unique guesses      
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 9 
    then do 
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
    else return ()
    
-- a way to exit the game after winning the game    
gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _) =
  if all isJust filledInSoFar 
    then do
      putStrLn $ "You are right, the word is " ++ (fmap toUpper wordToGuess) ++ ". You Win!"
      exitSuccess
    else return ()

-- the instruction for running the game
-- forever is used so that it will execute this series of actions indefenitely    
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter:  "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"
    
main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
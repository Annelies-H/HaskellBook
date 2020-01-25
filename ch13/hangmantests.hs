import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)

-- the Puzzle datatype includes the word we're trying to guess, a list of characters correctly guessed so far 
-- and a list of the characters guess so far
data Puzzle = Puzzle String [Maybe Char] [Char]

-- our own show instance makes sure that it does not show the word that needs to be guessed
-- it shows only the correctly guessed characters (aka discovered) and our other guesses (guessed)
instance Show Puzzle where
  show (Puzzle _ discovered guessed) = 
    (intersperse ' ' $ fmap renderPuzzleChar discovered) 
     ++ " Guessed so far:  " ++ guessed

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

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c:s)
  where
    zipper guessed wordChar guessChar = if wordChar == guessed
                                        then Just wordChar
                                        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "You guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"       
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False,_) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)
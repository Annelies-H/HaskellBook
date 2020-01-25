module Person where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Exit (exitSuccess)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show


data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                  "Name was: " ++ show name ++
                  " Age was: " ++ show age

printPerson :: Either PersonInvalid Person -> IO ()
printPerson (Right person) = putStrLn $ "Yay! Succesfully got a person: " ++ show person
printPerson (Left error) = putStrLn $ "Oh no! The following error occured: " ++ show error

checkName :: String -> IO ()
checkName string = if length noAlphabet == 0 
                    then return ()
                    else do
                      putStrLn "Only normal letters allowed. Program will quit."
                      exitSuccess
  where 
    noAlphabet = (filter (\x -> notElem x alphabet) string)
    alphabet = ['a'..'z'] ++ ['A'..'Z']

checkAge age = case (readMaybe age :: Maybe Integer) of
  Just a -> return ()
  Nothing -> do putStrLn "That is not a number. Program will quit."
                exitSuccess
        
                  
gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please give a name: "
  name <- getLine
  checkName name
  putStrLn "Please give an age: "
  age <- getLine
  checkAge age
  let person = mkPerson name (read age)
  printPerson person
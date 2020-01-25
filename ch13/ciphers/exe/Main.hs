module Main where

import Ceasar
import Vigenere
import Data.Maybe
import Text.Read
import System.Exit (exitSuccess)

--Ceasar cipher
ceasar' :: String -> String -> IO ()
ceasar' text key = do
  case readMaybe key of
    Just i -> do 
            putStrLn "You converted text is: " 
            putStrLn $ ceasar i text
    Nothing -> putStrLn "Only numbers are accepted. Your text was not converted."

unCeasar' :: String -> String -> IO ()
unCeasar' text key = do
  case readMaybe key of
    Just i -> do 
            putStrLn "You converted text is: " 
            putStrLn $ unCeasar i text
    Nothing -> putStrLn "Only numbers are accepted. Your text was not converted."   

    
-- Vigenere cipher
acceptedKey :: String
acceptedKey = ['a'..'z'] ++ " " ++ ['A'..'Z']

checkKey :: String -> Bool
checkKey [] = False
checkKey key = go key
  where
    go [] = True
    go (x:xs) = case elem x acceptedKey of
                  True -> go xs
                  False -> False    
                 
vigenere' :: String -> String -> IO ()
vigenere' text key = do
  if checkKey key 
    then do 
      putStrLn "You converted text is: " 
      putStrLn $ vigenere text key
    else do
      putStrLn "The key must contain one or more words. Each containing only characters from the normal alphabet."
      putStrLn "Your text was not converted."

unVigenere' :: String -> String -> IO ()
unVigenere' text key = do
  if checkKey key 
    then do 
      putStrLn "You converted text is: " 
      putStrLn $ unVigenere text key
    else do
      putStrLn "The key must contain one or more words. Each containing only characters from the normal alphabet."
      putStrLn "Your text was not converted."

-- Main
askOptions :: IO ()
askOptions = do
  putStrLn "Please choose an option"
  putStrLn "1 - Ceasar cipher"
  putStrLn "2 - revert Ceasar cipher"
  putStrLn "3 - Vigenere cipher"
  putStrLn "4 - revert Vigenere cipher"
  
nrOfOptions :: Int
nrOfOptions = 4

checkOption :: String -> IO ()
checkOption option = case isJust (readMaybe option :: Maybe Int) of
  False -> putStrLn "Sorry, that is not an option."
  True -> if (read option :: Int) <= 4 && (read option :: Int) > 0
            then return ()
            else do
              putStrLn "Sorry, that is not an option."
              exitSuccess

pickOption :: Int -> String -> String -> IO ()  
pickOption 1 text key = ceasar' text key
pickOption 2 text key = unCeasar' text key
pickOption 3 text key = vigenere' text key
pickOption 4 text key = unVigenere' text key
pickOption _ text key = putStrLn "Whoops, something went wrong."

main :: IO ()
main = do
  askOptions
  option <- getLine
  checkOption option
  putStrLn "Please input your text: "
  text <- getLine
  putStrLn "Please input your key: " 
  key <- getLine
  pickOption (read option) text key

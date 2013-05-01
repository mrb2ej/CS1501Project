module Main where

import System.IO
import Data.Function

main = do
  putStrLn "Please enter the name of the txt file containg your INTERCAL program:"
  answer <- getLine
  theInput <- readFile answer
  putStrLn (correctPlease (findPlease (words theInput)) (countLines theInput))

getNumToAdd :: Int -> Int -> Int
getNumToAdd numPleases numLines = ceiling ((calcRatio numLines 4)-(fromIntegral numPleases))

getNumToDelete :: Int -> Int -> Int
getNumToDelete numPleases numLines = ceiling ((fromIntegral numPleases)-(calcRatio numLines 3))

isUnderLimit :: Int -> Int -> Bool
isUnderLimit numPleases numLines = if((fromIntegral numPleases) < (calcRatio numLines 4)) then True else False

isOverLimit :: Int -> Int -> Bool
isOverLimit numPleases numLines = if((fromIntegral numPleases) > (calcRatio numLines 3)) then True else False

getCorrectNum :: Int -> Int -> String
getCorrectNum numPleases numLines = if(isUnderLimit numPleases numLines) 
	then ("You need to add " ++ (show (getNumToAdd numPleases numLines)) ++ " PLEASE's to your program!") 
	else if(isOverLimit numPleases numLines) 
	then ("You need to delete " ++ (show (getNumToDelete numPleases numLines)) ++ " PLEASE's from your program!") 
	else ""

findPlease :: [String] -> Int
findPlease []  = 0
findPlease (a:allWords) = if(a == "PLEASE") 
	then 1+(findPlease allWords) 
	else findPlease allWords
					
countLines :: String -> Int
countLines str = (length (lines str))

calcRatio :: Int -> Int -> Float
calcRatio x y = ((fromIntegral x) / (fromIntegral y))

correctPlease :: Int -> Int -> String
correctPlease please line = if((calcRatio please line) >= 0.33) then ("Your code is too polite!\n" ++ (getCorrectNum please line))
	else if((calcRatio please line) <= 0.25) then ("Your code is too rude!\n" ++ (getCorrectNum please line))
	else "You are perfectly polite! :)"
module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO


type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "../data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
          let l = length w
          in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed
    where renderPuzzleChar :: Maybe Char -> Char
          renderPuzzleChar Nothing = '_'
          renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle wd = Puzzle wd (map (const Nothing) wd) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wd _ _) c = elem c wd

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ gs) c = elem c gs

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSofar s) c =
  Puzzle word newFilledInSofar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSofar = zipWith (zipper c) word filledInSofar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess , alreadyGuessed puzzle guess) of
    (_, True) -> do
        putStrLn "You already guessed that character, pick something else!"
        return puzzle
    (True, _) -> do
        putStrLn "This character was in the word, filling in the word accordingly"
        return (fillInCharacter puzzle guess)
    (False, _) -> do
        putStrLn "This character wasn't in the word, try again."
        return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7 then
    do putStrLn "You Lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else
    return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSofar _) =
  if all isJust filledInSofar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  hSetBuffering stdout NoBuffering
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single charactr"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

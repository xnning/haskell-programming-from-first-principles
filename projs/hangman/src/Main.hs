module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

newtype WordList = WordList [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "../data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  WordList aw <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length w
          in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed _) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed
    where renderPuzzleChar :: Maybe Char -> Char
          renderPuzzleChar Nothing = '_'
          renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle wd = Puzzle wd (map (const Nothing) wd) [] []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wd _ _ _) c = elem c wd

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ gs _) c = elem c gs

fillInCharacter :: Puzzle -> Char -> Bool -> Puzzle
fillInCharacter (Puzzle word filledInSofar s incorrect) c correct =
  if correct
  then Puzzle word newFilledInSofar (c : s) incorrect
  else Puzzle word newFilledInSofar (c : s) (c: incorrect)
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
        return (fillInCharacter puzzle guess True)
    (False, _) -> do
        putStrLn "This character wasn't in the word, try again."
        return (fillInCharacter puzzle guess False)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed incorrect) =
  if (length incorrect) > 7 then
    do putStrLn "You Lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else
    return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSofar _ _) =
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

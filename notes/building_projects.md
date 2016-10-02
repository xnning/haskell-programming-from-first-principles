Bulding projects
----------------

Contents
========

-   writing Haskell programs with modules
-   using the Cabal package manager
-   building our project with Stack
-   conventions around Haskell project organization
-   building a small interactive game

Notes
=====

-   The Haskell Cabal, or Common Architecture for Building Applications and Libraries, is a package manager.

-   Stack is a cross-platform program for developing Haskell projects.
    -   The stack.yaml file is used to determine the versions of your packages and what version of GHC they’ll work best with.
-   Modules
    -   The effect of multiple import declarations is cumulative, but the ordering of import declarations is irrelevant.

Extra
=====

``` sourceCode
import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isLetter)
import System.IO
```

Exercises
=========

**Intermission: Check your understanding**

1.  forever, when
2.  Data.Bits, Database.Blacktip.Types.
3.  the main data constructors used in blacktip.
4.  code comparison
    1.  Control.Concurrent.MVar; Filesystem.Path.CurrentOS; Control.Concurrent
    2.  Filesystem
    3.  Control.Monad

**Hangman game logic**

see the code.

**Modifying code**

``` sourceCode
-- 2
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

-- 3
palindrome2 :: IO ()
palindrome2 = forever $ do
  line1 <- getLine
  let line2 = map toLower . filter isLetter $ line1
  case (line2 == reverse line2) of
    True -> putStrLn "It's a palindrome!"
    False ->putStrLn "Nope!"

-- 4
type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
 hSetBuffering stdout NoBuffering
 putStr "Give me the name:"
 name <- getLine
 putStr "Give me the age:"
 age <- fmap read getLine
 let person = mkPerson name age
 case person of
    Right p -> putStrLn $ "Yay! Successfully got a person: " ++ show p
    Left error -> putStrLn $ "Error: " ++ show error
```

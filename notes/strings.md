# Strings

## Exercises

### Reading syntax
1. b) (++) [1, 2, 3] [4, 5, 6]
d) ["hello" ++ " world"]
e) "hello" !! 4
g) take 4 "lovely"
2. ad; bc; ce; da; eb

### Building functions

1. a) "Curry is awesome" ++ "!"
   b) drop 4 (take 5 "Curry is awesome!")
   c) drop 9 "Curry is awesome!"

3.
```haskell
thirdLetter :: String -> Char
thirdLetter x = drop 2 (take 3 x)
```
4.
```haskell
letterIndex :: Int -> Char
letterIndex x = drop (x-1) (take x "Curry is awesome!" )
```
5.
```haskell
rvrs :: String -> String
rvrs s = (drop 9 s) ++ (take 4 (drop 5 s)) ++ (take 5 s) ++ "."
```
6.
```haskell
module Reverse where

rvrs :: String -> String
rvrs x = (drop 9 s) ++ (take 4 (drop 5 s)) ++ (take 5 s) ++ "."

main :: IO ()
main = print $ rvrs "Curry is awesome"
```

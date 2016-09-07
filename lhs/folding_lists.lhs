Folding lists
-------------------------

- One initially non-obvious aspect of folding is that it happens in two stages, traversal and folding.
    - traversal is the stage in which the fold recurses over the spine.
    - folding refers to the evaluation or reduction of the folding function applied to the values.

- Folds recurse over the spine in the same direction; the difference is in the association, or parenthesization, of the folding function.

- `Foldr` can avoid evaluating not just some or all of the values in the list, but some or all of the list's spine as well. For this reason, foldr can be used with lists that are potentially infinite.

- In `Foldl`, recursion of the spine is unconditional. This feature means that foldl is generally inappropriate with lists that are or could be infinite; it is also usually inappro- priate even for long lists, as it accumulates a pile of unevaluated values as it traverses the spine.

- In most cases, when you need a left fold, you should use foldl', which works the same except it is strict.

- For finite lists, `foldr f z xs = foldl (flip f) z (reverse xs)`

- A way to write fibs using scan: `fibs = 1 : scanl (+) 1 fibs`


Exercises
=========================

**Exercises: Undertanding Folds**

1. bc
2. 3 * (2 * (1 * 1))
3. c
4. a
5. fixed:
    a. `foldr (++) "" ["woot", "WOOT", "woot"]`
    b. `foldr max 'a' "fear is the little death"`
    c. `foldr (&&) True [False, True]`
    d. `foldr (||) False [False, True]`
    e. `foldl (\acc x -> acc ++ (show x)) "" [1..5]`
    f. `foldr (flip const) 'a' [1..5]`
    g. `foldr (flip const) 0 "tacos"`
    h. `foldl const 0 "burritos"`
    i. `foldl const 'z' [1..5]`

**Exercises: Database Processing**

> import Data.Time
>
> data DatabaseItem = DbString String
>                   | DbNumber Integer
>                   | DbDate   UTCTime
>                   deriving (Eq, Ord, Show)
>
> theDatabase :: [DatabaseItem]
> theDatabase =
>    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
>    , DbNumber 9001
>    , DbString "Hello, world!"
>    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
>    ]
>
> -- 1
> filterDbDate :: [DatabaseItem] -> [UTCTime]
> filterDbDate ls = [ utc | DbDate utc <- ls]
>
> -- 2
> filterDbNumber :: [DatabaseItem] -> [Integer]
> filterDbNumber ls = [num | DbNumber num <- ls]
>
> -- 3
> mostRecent :: [DatabaseItem] -> UTCTime
> mostRecent ls = foldr max (head times) times
>   where times = filterDbDate ls
>
> -- 4
> sumDb :: [DatabaseItem] -> Integer
> sumDb = sum . filterDbNumber
>
> -- 5
> avgDb :: [DatabaseItem] -> Double
> avgDb ls = (fromIntegral (sum nums)) / (fromIntegral (length nums))
>    where nums = filterDbNumber ls

**Scans Exercises**

> fibs = 1 : scanl (+) 1 fibs
>
> -- 1
> fibsTake = take 20 fibs
>
> -- 2
> fibsLess = takeWhile (< 100) fibs
>
> -- 3
> factorial = scanl (*) 1 [1..]

**Warm-up and review**

> -- 1
> stops  = "pbtdkg"
> vowels = "aeiou"
>
> sVS = [ [s1,v,s2] | s1 <- stops, v <- vowels, s2 <- stops]
>
> sVSP = [['p', v, s] | v <- vowels, s <- stops]
>
> -- 2. average length of each word in a string.
>
> -- 3
> seekritFunc x = fromIntegral ws / (fromIntegral wl)
>      where ws = sum (map length (words x))
>            wl = length (words x)

**Rewriting functions using folds**

> -- 1
> myOr :: [Bool] -> Bool
> myOr = foldr (||) False
>
> -- 2
> myAny :: (a -> Bool) -> [a] -> Bool
> myAny f ls = myOr $ map f ls
>
> -- 3
> myElem :: Eq a => a -> [a] -> Bool
> myElem a = myAny (==a)
>
> -- 4
> myReverse :: [a] -> [a]
> myReverse = foldl (\b a -> a : b) []
>
> -- 5
> myMap :: (a -> b) -> [a] -> [b]
> myMap f = foldr (\a b -> f a : b) []
>
> -- 6
> myFilter :: (a -> Bool) -> [a] -> [a]
> myFilter f = foldr (\a b -> if f a then a : b else b) []
>
> -- 7
> squish :: [[a]] -> [a]
> squish = foldr (++) []
>
> -- 8
> squishMap :: (a -> [b]) -> [a] -> [b]
> squishMap f = foldr (\a b -> f a ++ b) []
>
> -- 9
> squishAgain :: [[a]] -> [a]
> squishAgain = squishMap id
>
> -- 10
> myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
> myMaximumBy f l = foldl (\b a -> if f b a == GT then b else a)
>                         (head l) l
>
> -- 11
> myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
> myMinimumBy f l = foldl (\b a -> if f b a == LT then b else a)
>                         (head l) l

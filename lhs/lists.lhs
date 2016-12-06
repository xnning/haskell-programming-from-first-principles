Lists
========================

- Evaluation of the list in this representation proceeds down the spine. Constructing the list when that is necessary, however, proceeds up the spine.

- `sprint` to print variables and see what has been evaluated already.

- Values in Haskell get reduced to weak head normal form by default.

- Values in Haskell get reduced to weak head normal form by default.
    - normal form: the expression is fully evaluated.
    - weak head normal form: the expression is only evaluated as far as is necessary to reach a data constructor.

Extra Load
------------------------

> import Data.Char

Exercises
------------------------

**Exercises: EnumFromTo**

> eft :: (Ord a, Enum a) => a -> a -> [a]
> eft f t = if f > t then []
>               else if f == t then [f]
>               else f : eft (succ f) t

> eftBool :: Bool -> Bool -> [Bool]
> eftBool = eft
>
> eftOrd :: Ordering -> Ordering -> [Ordering]
> eftOrd = eft
>
> eftInt :: Int -> Int -> [Int]
> eftInt = eft
>
> eftChar :: Char -> Char -> [Char]
> eftChar = eft

**Exercises: Thy Fearful Symmetry**

> -- 1.
> myWords :: String -> [String]
> myWords [] = []
> myWords s = cur : myWords rest
>   where cur = takeWhile (/= ' ') s
>         rest = dropWhile (== ' ') . dropWhile (/= ' ') $ s
>
> -- 2.
> myLines :: String -> [String]
> myLines [] = []
> myLines s = cur : myLines rest
>   where cur = takeWhile (/= '\n') s
>         rest = dropWhile (== '\n') . dropWhile (/= '\n') $ s
>
> -- 3.
> myBreak :: Char -> String -> [String]
> myBreak _ [] = []
> myBreak c s = cur : myBreak c rest
>   where cur = takeWhile (/= c) s
>         rest = dropWhile (== c) . dropWhile (/= c) $ s

**Exercises: Bottom Madness**

1. no.
2. yes.
3. no.
4. yes.
5. no.
6. yes.
7. no.
8. yes.
9. yes.
10. no.

1. whnf. nf.
2. whnf.
3. neither.
4. neither.
5. neither.
6. neither
7. whnf.

**Exercises: More Bottoms**

1. bottom
2. 2
3. bottom
4. return a list of boolean which indicates the character is in "aeiou" or not.
5. [1,4,9,16,25,36,49,64,81,100]; [1,10,20]; [15,15,15]
6. `map (\x -> bool x (-x) (x == 3)) [1..10]`

**Exercises: Filtering**

> -- 1.
> filterThree = filter (\x -> mod x 3 == 0)
>
> -- 2.
> lengthThree = length . filter (\x -> mod x 3 == 0)
>
> -- 3.
> removeArticles ls = [x | x <- words ls, x/="a", x/="an", x/="the"]

**Zipping exercises**

> -- 1.
> myZip [] _ = []
> myZip _ [] = []
> myZip (x:xs) (y:ys) = (x,y): zip xs ys
>
> -- 2.
> myZipWith _ [] _ = []
> myZipWith _ _ [] = []
> myZipWith f (x:xs) (y:ys) = f x y: myZipWith f xs ys
>
> -- 3.
> myZip2 = myZipWith (\x y -> (x, y))

Chapter Exercises
------------------------

**Data.Char**

> -- 2.
> filterUpper = filter isUpper
>
> -- 3.
> capitalizeString [] = []
> capitalizeString (x:xs) = toUpper x : xs

> -- 4.
> capitalizeStringRec [] = []
> capitalizeStringRec (x:xs) = toUpper x : capitalizeStringRec xs
>
> capitalizeHead = toUpper. head

**Ciphers**

> caesarChar num char = chr (ord 'a' + shift)
>    where distance = ord char - ord 'a'
>          shift = (distance + num) `mod` 26
>
> caesar num str = map (caesarChar num) str
>
> unCaesar num str = map (caesarChar (26 - (mod num 26))) str

**Writing your own standard functions**

> -- 1
> myOr :: [Bool] -> Bool
> myOr [] = False
> myOr (x:xs) = x || myOr xs
>
> -- 2
> myAny :: (a -> Bool) -> [a] -> Bool
> myAny f [] = False
> myAny f (x:xs) = f x || myAny f xs
>
> -- 3
> myElem :: Eq a => a -> [a] -> Bool
> myElem _ [] = False
> myElem e (x:xs) = e == x || myElem e xs
>
> myElem2 :: Eq a => a -> [a] -> Bool
> myElem2 e xs = myAny (==e) xs
>
> -- 4
> myReverse :: [a] -> [a]
> myReverse [] = []
> myReverse (x:xs) = myReverse xs ++ [x]
>
> -- 5
> squish :: [[a]] -> [a]
> squish [] = []
> squish (x:xs) = x ++ (squish xs)
>
> -- 6
> squishMap :: (a -> [b]) -> [a] -> [b]
> squishMap _ [] = []
> squishMap f (x:xs) = f x ++ squishMap f xs
>
> -- 7
> squishAgain :: [[a]] -> [a]
> squishAgain = squishMap id
>
> -- 8
> myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
> myMaximumBy comp (x:xs) = go comp xs x
>  where go _ [] cur = cur
>        go comp (x:xs) cur = go comp xs (if comp cur x == GT then cur else x)
>
> -- 9
> myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
> myMinimumBy comp (x:xs) = go comp xs x
>  where go _ [] cur = cur
>        go comp (x:xs) cur = go comp xs (if comp cur x == LT then cur else x)
>
> myMaximum :: (Ord a) => [a] -> a
> myMaximum = myMaximumBy compare
>
> myMinimum :: (Ord a) => [a] -> a
> myMinimum = myMinimumBy compare

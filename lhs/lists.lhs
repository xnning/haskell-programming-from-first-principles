Lists
---------------

Exercises
===============

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

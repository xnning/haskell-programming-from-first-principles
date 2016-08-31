Lists
---------------

- Evaluation of the list in this representation proceeds down the spine. Constructing the list when that is necessary, however, proceeds up the spine.

- `sprint` to print variables and see what has been evaluated already.

- Values in Haskell get reduced to weak head normal form by default.

- Values in Haskell get reduced to weak head normal form by default.
    - normal form: the expression is fully evaluated.
    - weak head normal form: the expression is only evaluated as far as is necessary to reach a data constructor.

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
> myBreak c s = cur : myBreak c rest >   where cur = takeWhile (/= c) s
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

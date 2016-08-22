Recursion
---------

-   We use a combinator - known as the Y combinator or fixed-point combinator - to write recursive functions in the lambda calculus.

-   Bottom is a term used in Haskell to refer to computations that do not successfully result in a value. The two main varieties of bottom are computations that failed with an error or those that failed to terminate.

-   A common Haskell idiom called a `go` function, which allows us to define a function via a where-clause that can accept more arguments than the top-level function does.

Exercises
=========

``` sourceCode
-- extra import used in exercises
import Data.List (intersperse)
```

**Review of types**

1.  d
2.  b
3.  d
4.  b

**Reviewing currying**

1.  "woops mrow woohoo!"
2.  "woops mrow 2 mrow haha"
3.  "woops mrow blue mrow haha"
4.  "pink mrow haha mrow green mrow woops mrow blue"
5.  "are mrow Pugs mrow awesome"

**Recursion**

``` sourceCode
-- 1. (15, 2, 0), (13, 2, 1), (11, 2, 2), (9, 2, 3), (7, 2, 4), (5, 2, 5), (3, 2, 6), (1, 2, 7)

-- 2.
sumRec :: (Eq a, Num a) => a -> a
sumRec n = if n == 1 then 1 else sumRec (n - 1) + n

-- 3.
mulRec :: (Integral a) => a -> a -> a
mulRec n m = go n m 0
  where go n m cur =
              if n < 1 then cur
              else go (n - 1) m (cur + m)
```

**Fixing dividedBy**

``` sourceCode
data DividedResult =
    Result Integer
  | DividedByZero deriving Show

dividedBy ::  Integral a => a -> a -> DividedResult
dividedBy m n = if n == 0 then DividedByZero
                else if m > 0 && n < 0 then Result (- (go m (-n) 0))
                else if n > 0 && m < 0 then Result (- (go (-m) n 0))
                else if n < 0 && m < 0 then Result (go (-m) (-n) 0)
                else Result (go m n 0)
  where go m n cur | n > m = cur
                   | otherwise = go (m - n) n (cur + 1)
```

**McCarthy 91 function**

``` sourceCode
mc91 :: (Num a, Ord a) => a -> a
mc91 n | n > 100 = n - 10
       | otherwise = mc91 (mc91 (n + 11))
```

**Numbers into words**

``` sourceCode
digitToWord :: Int -> String
digitToWord n = lst !! n
  where lst = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digits :: Int -> [Int]
digits n = go n []
  where go n cur | n == 0 = cur
                 | otherwise = go (div n 10) (mod n 10 : cur)

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map digitToWord . digits $ n
```

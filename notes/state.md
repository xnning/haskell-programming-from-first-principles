State
-----

Contents
========

-   talk about what state means
-   explore some ways of handling state in Haskell
-   generate some more random numbers
-   and examine the State newtype and Monad instance

Extras
======

``` haskell
{-# LANGUAGE InstanceSigs #-}
import System.Random
```

Notes
=====

-   State

``` haskell
newtype State s a =
  State { runState :: s -> (a, s) }
```

-   Newtypes must have the same underlying representation as the type they wrap, so the function contained in the newtype must be isomorphic to the type it wraps: that is, there must be a way to go from the newtype to the thing it wraps and back again without losing information.

**Write State for yourself**

``` haskell
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, st) = g s in (f a, st)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi ((,) a)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (ab, s1) = f s
                                        (a, s2) = g s1
                                    in (ab a, s2)

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (a, st) = f s
                              in runMoi (g a) st

```

Exercises
=========

**Exercises: Roll Your Own**

``` haskell
data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    _ -> error $ "must be 1-6"

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= n = count
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
              in go (sum + die) (count + 1) nextGen


rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go sum (count, dies) gen
          | sum >= n = (count, dies)
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
              in go (sum + die) (count + 1, intToDie die:dies) nextGen
```

**Fizzbuzz Differently**

``` haskell
fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 ="Fizz"
           | n `mod` 3  == 0 ="Buzz"
           | otherwise       = show n

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = go from to []
  where go :: Integer -> Integer -> [String] -> [String]
        go from to strs | to >= from = strs
                        | otherwise  = go from (to - 1) (fizzBuzz to : strs)
```

Chapter Exercises
=================

``` haskell
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \s' -> ((), s)

exec :: State s a -> s -> s
exec (State sa) = snd . sa

eval :: State s a -> s -> a
eval (State sa) = fst . sa

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
```

Monad
-----

Contents
========

-   define Monad, its operations and laws
-   look at several examples of monads in practice
-   write the Monad instances for various types
-   address some misinformation about monads

Extra
=====

``` haskell
import Control.Monad (join)
```

Notes
=====

-   A Monad is an applicative functor. Monad is stronger than Applicative, and Applicative is stronger than Functor.

``` haskell
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a

-- define Functor in items of Monad
fmap_m :: (Monad m) => (a -> b) -> (m a) -> (m b)
fmap_m f xs = xs >>= return . f

-- define Applicative in terms of Monad
pure_m :: (Monad m) => a -> m a
pure_m = return

ap_m :: (Monad m) => m (a -> b) -> m a -> m b
ap_m f xs = xs >>= (\x -> f >>= (\g -> return (g x)))
```

-   The unique part of Monad is the following function:

``` haskell
-- join :: Monad m => m (m a) -> m a
```

-   Define bird operation in terms of fmap and join:

``` haskell
bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join (fmap f x)
```

-   With the Maybe Applicative, each Maybe computation fails or succeeds independently of each other; With the Maybe Monad, computations contributing to the final result can choose to return Nothing based on "previous" computations.

-   Laws:
    -   identity: return should be neutral and not perform any computation.
        -   right identity: `m >> return = m`
        -   left identity: `return x >> f = f x`
    -   associativity: `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`
-   Kleisli composition.

``` haskell
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
```

Exercises
=========

**Short Exercise: Either Monad**

``` haskell
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure a = Second a
  (<*>) (First n) _ = First n
  (<*>) _ (First n) = First n
  (<*>) (Second f) (Second x) = Second (f x)

instance Monad (Sum a) where
  return = pure
  (>>=) (First n) _ = First n
  (>>=) (Second n) f = f n
```

Chapter Exercises
=================

``` haskell
-- 1
data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

-- 2

data PhhhbbtttEither b a =
    Left' a
  | Right' b

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap f (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure x = Left' x
  (<*>) (Right' b) _ = Right' b
  (<*>) _ (Right' b) = Right' b
  (<*>) (Left' f) (Left' x) = Left' (f x)

instance Monad (PhhhbbtttEither b) where
  (>>=) (Right' b) _ = Right' b
  (>>=) (Left' x) f = f x

-- 3
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

-- 4
data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) fs xs = flatMap (\f -> fmap f xs) fs

instance Monad List where
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = append (f x) (xs >>= f)
```

``` haskell
-- 1
j :: Monad m => m (m a) -> m a
j x = x >>= id

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = x >>= (return . f)

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = a >>= (\a1 -> b >>= (\b1 -> return (f a1 b1)))

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a x f = x >>= (\x1 -> f >>= (\f1 -> return (f1 x1)))

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) f = l2 (:) (f x) (meh xs f)

-- 6
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
```

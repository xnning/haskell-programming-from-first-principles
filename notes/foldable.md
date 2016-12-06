Foldable
========

Contents
--------

-   the Foldable class and its operations
-   the monoidal nature of folding
-   standard operations derived from folding

Extras
------

``` haskell
import Data.Foldable
import Data.Monoid
```

Notes
-----

-   the foldable class: class of data structures that can be folded to a summary value.

``` haskell
class Foldable' t where
  {-# MINIMAL foldMap' | foldr' #-}
```

-   Importance of Monoid: folding necessarily implies a binary associative operation that has an identity value

``` haskell
  fold' :: Data.Monoid.Monoid m => t m -> m
  foldMap' :: Data.Monoid.Monoid m => (a -> m) -> t a -> m
  foldr' :: (a -> b -> b) -> b -> t a -> b
  {- ... more ... -}
```

Exercises
---------

**Exercises: Library Functions**

``` haskell
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct. foldMap Product

-- all :: Foldable t => (a -> Bool) -> t a -> Bool
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem = any . (==)

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr go Nothing
  where go :: (Ord a) => a -> Maybe a -> Maybe a
        go n Nothing = Just n
        go n (Just e) = Just (min n e)

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr go Nothing
  where go :: (Ord a) => a -> Maybe a -> Maybe a
        go n Nothing = Just n
        go n (Just e) = Just (max n e)

null :: (Foldable t) => t a -> Bool
null = foldr (\x y -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ acc -> acc + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = foldr (mappend . f) mempty
```

Chapter Exercises
-----------------

``` haskell
data Constant a b = Constant a

instance Foldable (Constant a) where
  foldMap f (Constant a) = mempty

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b1 b2) = (f b1) <> (f b2)

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b1 b2 b3) = (f b1) <> (f b2) <> (f b3)
```

``` haskell
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
```

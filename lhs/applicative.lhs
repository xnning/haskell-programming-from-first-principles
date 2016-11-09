Applicative
------------------------

Contents
========================

- define and explore the Applicative typeclass and its core operations
- demonstrate why applicatives are monoidal functors
- make the usual chitchat about laws and instances
- do a lot of lifting
- give you some Validation

Notes
========================

- Applicative is where the function we are applying is also embedded in some structure.

> -- class Functor f => Applicative f where
> --   pure :: a -> f a
> --   (<*>) :: f (a -> b) -> f a -> f b

- Type has Applicative must have Functor.

> -- fmap f x = pure f <*> x

- Applicative functors are monoidal functors. the `f` is  the monodial part, and the map behavior is the functorial part.

- Laws
    - Identity: `pure id <*> v = v`
    - Composition: `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
    - Homomorphism: `pure f <*> pure x = pure (f x)`
    - Interchange:: `u <*> pure y = pure ($ y) <*> u
`


Extra
========================

> import Data.List (elemIndex)
> import Control.Applicative (liftA2, liftA3)

Exercises
========================

**Exercises: Lookups**

> -- 1
> added :: Maybe Integer
> added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
>
> -- 2
> y :: Maybe Integer
> y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
>
> z :: Maybe Integer
> z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]
>
> tupled :: Maybe (Integer, Integer)
> tupled = (,) <$> y <*> z
>
> -- 3
> x :: Maybe Int
> x = elemIndex 3 [1, 2, 3, 4, 5]
>
> y1 :: Maybe Int
> y1 = elemIndex 4 [1, 2, 3, 4, 5]
>
> max' :: Int -> Int -> Int
> max' = max
>
> maxed :: Maybe Int
> maxed = max' <$> x <*> y1
>
> -- 4
>
> xs = [1, 2, 3]
> ys = [4, 5, 6]
>
> x2 :: Maybe Integer
> x2 = lookup 3 $ zip xs ys
>
> y2 :: Maybe Integer
> y2 = lookup 2 $ zip xs ys
>
> summed :: Maybe Integer
> summed = fmap sum $ (,) <$> x2 <*> y2

**Exercise: Identity Instance**

> newtype Identity a = Identity a
>   deriving (Eq, Ord, Show)
>
> instance Functor Identity where
>   fmap f (Identity a) = Identity (f a)
>
> instance Applicative Identity where
>   pure = Identity
>   (<*>) (Identity f) (Identity x) = Identity (f x)

**Exercise: Constant Instance**

> newtype Constant a b = Constant { getConstant :: a }
>   deriving (Eq, Ord, Show)
>
> instance Functor (Constant a) where
>   fmap _ (Constant a) = Constant a
>
> instance Monoid a => Applicative (Constant a) where
>   pure _ = Constant (mempty)
>   (<*>) (Constant e1) (Constant e2) = Constant (mappend e1 e2)

**Exercise: Fixer Upper**

> -- 1
> e1 = const <$> Just "Hello" <*> pure "World"
>
> -- 2
> e2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

**List Applicative Exercise**

> data List a = Nil | Cons a (List a) deriving (Eq, Show)
>
> instance Functor List where
>   fmap f Nil = Nil
>   fmap f (Cons a as) = Cons (f a) (fmap f as)
>
> append :: List a -> List a -> List a
> append Nil ys = ys
> append (Cons x xs) ys = Cons x $ xs `append` ys
>
> fold :: (a -> b -> b) -> b -> List a -> b
> fold _ b Nil = b
> fold f b (Cons h t) = f h (fold f b t)
>
> concat' :: List (List a) -> List a
> concat' = fold append Nil
>
> flatMap :: (a -> List b) -> List a -> List b
> flatMap f as = concat' (fmap f as)
>
> instance Applicative List where
>   pure a = Cons a Nil
>   (<*>) Nil _ = Nil
>   (<*>) fs xs = flatMap (\f -> fmap f xs) fs

**ZipList Applicative Exercise**

> newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)
>
> instance Functor ZipList' where
>   fmap f (ZipList' xs) = ZipList' $ fmap f xs
>
>
> repeat' :: a -> List a
> repeat' x = Cons x (repeat' x)
>
> h :: List (a -> b) -> List a -> List b
> h Nil _ = Nil
> h _ Nil = Nil
> h (Cons f fs) (Cons x xs) = Cons (f x) (h fs xs)
>
> instance Applicative ZipList' where
>   pure = ZipList' . repeat'
>   (<*>) (ZipList' fs) (ZipList' xs) = ZipList' (h fs xs)
>
> instance Monoid a => Monoid (ZipList' a) where
>   mempty = pure mempty
>   mappend = liftA2 mappend

**Exercise: Variations on Either**

> data Sum a b = First a | Second b deriving (Eq, Show)
>
> data Validation e a = Error e | Success a deriving (Eq, Show)
>
> instance Functor (Sum a) where
>   fmap _ (First a) = First a
>   fmap f (Second b) = Second (f b)
>
> instance Applicative (Sum a) where
>   pure = Second
>   (<*>) (First a) _ = First a
>   (<*>) _ (First b) = First b
>   (<*>) (Second f) (Second a) = Second (f a)
>
> instance Functor (Validation e) where
>   fmap _ (Error e) = Error e
>   fmap f (Success a) = Success (f a)
>
> instance Monoid e => Applicative (Validation e) where
>   pure = Success
>   (<*>) (Error e1) (Error e2) = Error (mappend e1 e2)
>   (<*>) (Error e1) _ = Error e1
>   (<*>) _ (Error e2) = Error e2
>   (<*>) (Success f) (Success e) = Success (f e)

Chapter Exercises
========================

1. []
    - pure :: a -> [a]
    - (<*>) :: [(a -> b)] -> [a] -> [b]
2. IO
    - pure ::a -> IO a
    - (<*>) :: IO (a -> b) -> IO a -> IO b
3. (,) a
    - pure :: Monoid b => a -> (b, a)
    - (<*>) :: Monoid c => (c, (a -> b)) -> (c, a) -> (c, b)

4. (->) e
    - pure :: a -> (e -> a)
    - (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)

> -- 1: already written before
>
> -- 2
> data Pair a = Pair a a deriving Show
>
> instance Functor Pair where
>   fmap f (Pair a b) = Pair (f a) (f b)
>
> instance Applicative Pair where
>   pure a = Pair a a
>   (<*>) (Pair f1 f2) (Pair x1 x2) = Pair (f1 x1) (f2 x2)
>
> -- 3
> data Two a b = Two a b
>
> instance Functor (Two a) where
>   fmap f (Two a b) = Two a (f b)
>
> instance (Monoid a) => Applicative (Two a) where
>   pure x = Two mempty x
>   (<*>) (Two a1 f) (Two a2 x) = Two (mappend a1 a2) (f x)
>
> -- 4
> data Three a b c = Three a b c
>
> instance Functor (Three a b) where
>   fmap f (Three a b c) = Three a b (f c)
>
> instance (Monoid a, Monoid b) => Applicative (Three a b) where
>   pure x = Three mempty mempty x
>   (<*>) (Three a1 b1 f) (Three a2 b2 x) = Three (mappend a1 a2) (mappend b1 b2) (f x)
>
> -- 5
> data Three' a b = Three' a b b
>
> instance Functor (Three' a) where
>   fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)
>
> instance (Monoid a) => Applicative (Three' a) where
>   pure x = Three' mempty x x
>   (<*>) (Three' b1 f1 f2) (Three' b2 x1 x2) = Three' (mappend b1 b2) (f1 x1) (f2 x2)
>
> -- 6
> data Four a b c d = Four a b c d
>
> instance Functor (Four a b c) where
>   fmap f (Four a b c d) = Four a b c (f d)
>
> instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
>   pure x = Four mempty mempty mempty x
>   (<*>) (Four a1 b1 c1 f) (Four a2 b2 c2 x) =  Four (mappend a1 a2) (mappend b1 b2) (mappend c1 c2) (f x)
>
> -- 7
> data Four' a b = Four' a a a b
>
> instance Functor (Four' a) where
>   fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)
>
> instance (Monoid a) => Applicative (Four' a) where
>   pure x = Four' mempty mempty mempty x
>   (<*>) (Four' a1 a2 a3 f) (Four' b1 b2 b3 x) = Four' (mappend a1 b1) (mappend a2 b2) (mappend a3 b3) (f x)

**Combinations**

> stops :: String
> stops = "pbtdkg"
>
> vowels :: String
> vowels = "aeiou"
>
> combos :: [a] -> [b] -> [c] -> [(a, b, c)]
> combos = liftA3 (,,) a b c

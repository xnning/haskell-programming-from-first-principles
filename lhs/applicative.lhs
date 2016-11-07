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

- Applicative functors are monoidal functors.

Extra
========================

> import Data.List (elemIndex)

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

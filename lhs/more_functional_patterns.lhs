More functional patterns
-------------------------

- use GHCi's `:browse` to see a list of the type signatures and functions we loaded from the module.

- Incomplete pattern matches applied to data they don't handle will return bottom, a non-value used to denote that the program cannot return a value or result.

- Pointfree refers to a style of composing functions without specifying their arguments.

Exercises
========================

**Multiple choice**

1. d
2. b
3. d
4. b
5. a

**Let's write code**

> -- 1.
> tensDigit :: Integral a => a -> a
> tensDigit x = d
>    where (xLast, _) = x `divMod` 10
>          (_, d) = xLast `divMod` 10
>
> hunsD :: Integral a => a -> a
> hunsD x = d2
>    where d = x `div` 100
>          d2 = d `mod` 100
>
> -- 2.
> foldBool :: a -> a -> Bool -> a
> foldBool a b c = case c of True -> a
>                            False -> b
>
> foldBool2 :: a -> a -> Bool -> a
> foldBool2 a b c
>       | c = a
>       | otherwise = b
>
> -- 3.
> g :: (a -> b) -> (a, c) -> (b, c)
> g f (a, c)= (f a, c)
>
> -- 5.
>
> roundTrip2 :: (Show a, Read a) => a -> a
> roundTrip2 = read. show
>
> -- 6.
>
> roundTrip3 :: (Show a, Read b) => a -> b
> roundTrip3 = read. show
>
> main = do
>   print ((roundTrip3 4) :: Int)
>   print (id 4)

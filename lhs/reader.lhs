Reader
------------------------

Contents
========================

- examine the Functor, Applicative, and Monad instances for functions
- learn about the Reader newtype
- see some examples of using Reader

Extra
========================

> {-# LANGUAGE InstanceSigs #-}
> import Data.Char
> import Control.Applicative
> import Control.Monad (liftM2)
> import Data.Maybe

Notes
========================

- Functions have Functor, Applicative, Monad instances. The term Reader refers to the Applicative or Monad instances: reading an argument from the environment into functions.

> newtype Reader r a =
>     Reader { runReader :: r -> a }

- The "read-only" nature of the type argument r means that you can swap in a different type or value of r for functions that you call, but not for functions that call you.

Exercises
========================

**Short Exercise: Warming Up**

> cap :: [Char] -> [Char]
> cap xs = map toUpper xs
>
> rev :: [Char] -> [Char]
> rev xs = reverse xs
>
> composed :: [Char] -> [Char]
> composed = rev . cap
>
> fmapped :: [Char] -> [Char]
> fmapped = fmap rev cap
>
> tupled :: [Char] -> ([Char], [Char])
> tupled = (,) <$> rev <*> cap

**Exercise: Ask**

> ask :: Reader a a
> ask = Reader id

**Exercise: Reading Comprehension**

> myLiftA2 :: Applicative f => (a -> b -> c)
>   -> f a -> f b -> f c
> myLiftA2 f fa fb = f <$> fa <*> fb
>
> asks :: (r -> a) -> Reader r a
> asks f = Reader f
>
> instance Functor (Reader r) where
>   fmap ab (Reader ra) = Reader $ ab . ra
>
> instance Applicative (Reader r) where
>   pure :: a -> Reader r a
>   pure a = Reader $ const a
>
>   (<*>) :: Reader r (a -> b)
>         -> Reader r a
>         -> Reader r b
>   (Reader rab) <*> (Reader ra) =
>     Reader $ \r -> rab r (ra r)
>
> newtype HumanName = HumanName String deriving (Eq, Show)
> newtype DogName = DogName String deriving (Eq, Show)
> newtype Address = Address String deriving (Eq, Show)
>
> data Person = Person {
>     humanName :: HumanName
>   , dogName :: DogName
>   , address :: Address
> } deriving (Eq, Show)
>
> data Dog = Dog {
>     dogsName :: DogName
>   , dogsAddress :: Address
> } deriving (Eq, Show)
>
> getDogR :: Reader Person Dog
> getDogR = Reader $ liftA2 Dog dogName address

**Exercise: Reader Monad**

> instance Monad (Reader r) where
>   return = pure
>
>   (>>=) :: Reader r a
>         -> (a -> Reader r b)
>         -> Reader r b
>   (>>=) (Reader ra) f = Reader $ \r -> ((runReader . f) . ra) r r
>
> getDogRM :: Reader Person Dog
> getDogRM = Reader $ liftM2 Dog dogName address

Chapter Exercises
========================

**A warm-up stretch**

> x = [1, 2, 3]
> y = [4, 5, 6]
> z = [7, 8, 9]
>
> xs :: Maybe Integer
> xs = lookup 3 (zip x y)
>
> ys :: Maybe Integer
> ys = lookup 6 (zip y z)
>
> zs :: Maybe Integer
> zs = lookup 4 (zip x y)
>
> z' :: Integer -> Maybe Integer
> z' n = lookup n (zip x z)
>
> x1 :: Maybe (Integer, Integer)
> x1 = liftA2 (,) xs ys
>
> x2 :: Maybe (Integer, Integer)
> x2 = liftA2 (,) ys zs
>
> x3 :: Integer -> (Maybe Integer, Maybe Integer)
> x3 = liftA2 (,) z' z'
>
> summed :: Num c => (c, c) -> c
> summed = uncurry (+)
>
> bolt :: Integer -> Bool
> bolt = liftA2 (&&) (>3) (<8)
>
> main :: IO ()
> main = do
>   print $ foldSequA 7
>   print $ sequA (fromMaybe 7 s')
>   print $ bolt (fromMaybe 7 ys)
>   print $ ((bolt . fromMaybe 7) <$> z') 3
>
> sequA :: Integral a => a -> [Bool]
> sequA m = sequenceA [(>3), (<8), even] m
>
> s' :: Maybe Integer
> s' = summed <$> ((,) <$> xs <*> ys)
>
> foldSequA :: Integral a => a -> Bool
> foldSequA = (foldr (&&) True) . sequA

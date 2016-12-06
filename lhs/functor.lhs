Functor
========================

Contents
------------------------

- the return of the higher-kind types
- fmaps galore, and not just on lists
- no more digressions about dusty logicians
- words about typeclasses and constructor classes
- puns based on George Clinton music, probably

Notes
------------------------

- A functor is a way to apply a function over or around some structure that we don’t want to alter.

> -- class Functor f where
> --   fmap :: (a -> b) -> f a -> f b

- functor laws
    - Identity: fmap id == id
    - Composition: fmap (f . g) == fmap f . fmap g

- Unlike Monoid, Functor instances will be unique for a datatype, in part because of parametricity, in part because arguments to type constructors are applied in order of definition. In a hypothetical not-Haskell language, other cases might be possible.

Extra
------------------------

> {-# LANGUAGE FlexibleInstances #-}

Exercises
------------------------

**Exercises: Be Kind**

1. `*`
2. `* -> *; * -> *`
3. `* -> * -> *`

**Exercises: Heavy Lifting**

1. `a = fmap (+1) $ read "[1]" :: [Int]`
2. `b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])`
3. `c = (*2) . (\x -> x - 2)`
4. `((return '1' ++) . show) . (\x -> [x, 1..3])`

> -- 5
> e :: IO Integer
> e = let ioi = readIO "1" :: IO Integer
>         changed = fmap (read . ("123"++) . show) ioi
>     in fmap (3 *) changed

**Exercises: Instances of Func**

> -- 1
> newtype Identity a = Identity a
> instance Functor Identity where
>   fmap f (Identity a) = Identity (f a)
>
> -- 2
> data Pair a = Pair a a
> instance Functor Pair where
>   fmap f (Pair a b) = Pair (f a) (f b)
>
> -- 3
> data Two a b = Two a b
> instance Functor (Two a) where
>   fmap f (Two a b) = Two a (f b)
>
> -- 4
> data Three a b c = Three a b c
> instance Functor (Three a b) where
>   fmap f (Three a b c) = Three a b (f c)
>
> -- 5
> data Three' a b = Three' a b b
> instance Functor (Three' a) where
>   fmap f (Three' a b c) = Three' a (f b) (f c)
>
> -- 6
> data Four a b c d = Four a b c d
> instance Functor (Four a b c) where
>   fmap f (Four a b c d) = Four a b c (f d)
>
> -- 7
> data Four' a b = Four' a a a b
> instance Functor (Four' a) where
>   fmap f (Four' a b c d) = Four' a b c (f d)
>
> -- 8. no. kind is not right.

**Exercise: Possibly**

> data Possibly a = LolNope
>                 | Yeppers a deriving (Eq, Show)
>
> instance Functor Possibly where
>   fmap f LolNope = LolNope
>   fmap f (Yeppers a) = Yeppers (f a)

**Short Exercise**

> data Sum a b = First a
>              | Second b deriving (Eq, Show)
> instance Functor (Sum a) where
>   fmap f (First a) = First a
>   fmap f (Second b) = Second (f b)

Chapter Exercises
------------------------

Determine if a valid Functor can be written for the datatype provided.

1. no
2. yes
3. yes
4. no
5. no

Rearrange the arguments to the type constructor of the datatype
so the Functor instance works.

> -- 1
> data Sum' a b = First' b
>              | Second' a
> instance Functor (Sum' e) where
>   fmap f (First' a) = First' (f a)
>   fmap f (Second' b) = Second' b
>
> -- 2
> data Company a b c = DeepBlue a b
>                    | Something c
> instance Functor (Company e e') where
>   fmap f (Something b) = Something (f b)
>   fmap _ (DeepBlue a c) = DeepBlue a c
>
> -- 3
> data More a b = L b a b
>               | R a b a deriving (Eq, Show)
> instance Functor (More x) where
>   fmap f (L a b a') = L (f a) b (f a')
>   fmap f (R b a b') = R b (f a) b'

Write Functor instances for the following datatypes.

> -- 1
> data Quant a b = Finance | Desk a | Bloor b
> instance Functor (Quant a) where
>   fmap f (Bloor b) = Bloor (f b)
>   fmap f Finance = Finance
>   fmap f (Desk a) = Desk a
>
> -- 2
> data K a b = K a
> instance Functor (K a) where
>   fmap _ (K a) = K a
>
> -- 3
> newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
> newtype K' a b = K' a
> instance Functor (Flip K' a) where
>   fmap f (Flip (K' a)) = Flip (K' (f a))
>
> -- 4
> data EvilGoateeConst a b = GoatyConst b
> instance Functor (EvilGoateeConst a) where
>   fmap f (GoatyConst b) = GoatyConst (f b)
>
> -- 5
> data LiftItOut f a = LiftItOut (f a)
> instance Functor f => Functor (LiftItOut f) where
>   fmap f (LiftItOut fa) = LiftItOut (fmap f fa)
>
> -- 6
> data Parappa f g a = DaWrappa (f a) (g a)
> instance (Functor f, Functor g) => Functor (Parappa f g) where
>   fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)
>
> -- 7
> data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
> instance (Functor g) => Functor (IgnoreOne f g a) where
>   fmap f (IgnoringSomething fa ga) = IgnoringSomething fa (fmap f ga)
>
> -- 8
> data Notorious g o a t = Notorious (g o) (g a) (g t)
> instance (Functor g) => Functor (Notorious g o a) where
>   fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)
>
> -- 9
> data List a = Nil | Cons a (List a)
> instance Functor List where
>   fmap _ Nil = Nil
>   fmap f (Cons a as) = Cons (f a) (fmap f as)
>
> -- 10
> data GoatLord a = NoGoat
>                 | OneGoat a
>                 | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
> instance Functor GoatLord where
>   fmap _ NoGoat = NoGoat
>   fmap f (OneGoat a) = OneGoat (f a)
>   fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)
>
> -- 11
> data TalkToMe a = Halt
>                 | Print String a
>                 | Read (String -> a)
> instance Functor TalkToMe where
>   fmap _ Halt = Halt
>   fmap f (Print s a) = Print s (f a)
>   fmap f (Read g) = Read (f . g)

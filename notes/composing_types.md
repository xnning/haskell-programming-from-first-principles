Composing types
===============

Contents
--------

-   domonstrate why composing two monads does not give you another monad
-   examine the Identity and Compose types
-   manipulate types until we can make monads compose
-   meet some common monad transformers
-   work through an Identity crisis

Extra
-----

``` haskell
{-# LANGUAGE InstanceSigs #-}
import Control.Applicative
```

Notes
-----

-   A monad transformer is a variant of an ordinary type that takes an additional type argument which is assumed to have a monad instance.

-   When you compose two Functors, you get another Functor. Also for Applicative.

``` haskell
newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose g) = Compose $ liftA2 (<*>) f g
```

-   But for Monads, we need to use Monad transformer. Example is IdentityT.

``` haskell
newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure = IdentityT . pure
  IdentityT f <*> IdentityT a = IdentityT (f <*> a)

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  IdentityT a >>= f = IdentityT $ a >>= (runIdentityT . f)
```

-   The basic pattern that many monad transformers are enabling us to cope with is the following type transitions, where T is the concrete type.

``` haskell
-- m (T m b)
-- m (m b)
-- m b
-- T (m b)
```

Exercises
---------

**Exercises: Compose Instances**

``` haskell
-- compose foldable

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = foldMap (foldMap f) fga

-- compose traversable

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga

-- now for something completely different

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b
instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const' a b = Const' a
instance Bifunctor Const' where
  bimap f g (Const' a) = Const' (f a)

data Drei a b c = Drei a b c
instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b
instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a
instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d
instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either' a b = Left' a | Right' b
instance Bifunctor Either' where
  first f (Left' a) = Left' (f a)
  first f (Right' b) = Right' b

  second g (Left' a) = Left' a
  second g (Right' b) = Right' (g b)
```

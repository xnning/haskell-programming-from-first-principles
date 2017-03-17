Monad transformers
==================

Contents
--------

-   work through more monad transformer types and instances
-   look at the ordering and wrapping of monad transformer stacks
-   lift, lift, lift, and lift some more

Extra
-----

``` haskell
import Control.Applicative (liftA2)
import Control.Monad (join)
```

Notes
-----

-   MaybeT: monad transformer for Maybe

``` haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ fmap (fmap f) ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure a = MaybeT $ pure (pure a)

  (MaybeT f) <*> (MaybeT a) = MaybeT $ liftA2 (<*>) f a

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> pure Nothing
      Just a  -> (runMaybeT . f) a
```

-   EitherT: monad transformer for Either

``` haskell
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT v) = EitherT $ fmap (fmap f) v

instance (Applicative m) => Applicative (EitherT e m) where
  pure a = EitherT . pure . pure $ a
  (EitherT f) <*> (EitherT a) = EitherT $ liftA2 (<*>) f a

instance (Monad m) => Monad (EitherT e m) where
  return = pure
  (EitherT m) >>= f = EitherT $ do
    v <- m
    case v of
      Left a -> return (Left a)
      Right b -> runEitherT . f $ b

swapEither :: (Functor m) => EitherT e m a -> EitherT a m e
swapEither (EitherT a) = EitherT $ fmap f a
  where f x = case x of
                Left a -> Right a
                Right a -> Left a

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT m) = join $ do
  v <- m
  return $ either f g v
```

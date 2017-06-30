Monad transformers
========================

Contents
------------------------

- work through more monad transformer types and instances
- look at the ordering and wrapping of monad transformer stacks
- lift, lift, lift, and lift some more

Extra
------------------------

> import Control.Applicative (liftA2)
> import Control.Monad (join)

Notes
------------------------

- MaybeT: monad transformer for Maybe

> newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
>
> instance (Functor m) => Functor (MaybeT m) where
>   fmap f (MaybeT ma) = MaybeT $ fmap (fmap f) ma
>
> instance (Applicative m) => Applicative (MaybeT m) where
>   pure a = MaybeT $ pure (pure a)
>
>   (MaybeT f) <*> (MaybeT a) = MaybeT $ liftA2 (<*>) f a
>
> instance (Monad m) => Monad (MaybeT m) where
>   return = pure
>   (MaybeT ma) >>= f = MaybeT $ do
>     v <- ma
>     case v of
>       Nothing -> pure Nothing
>       Just a  -> (runMaybeT . f) a

- EitherT: monad transformer for Either

> newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
>
> instance (Functor m) => Functor (EitherT e m) where
>   fmap f (EitherT v) = EitherT $ fmap (fmap f) v
>
> instance (Applicative m) => Applicative (EitherT e m) where
>   pure a = EitherT . pure . pure $ a
>   (EitherT f) <*> (EitherT a) = EitherT $ liftA2 (<*>) f a
>
> instance (Monad m) => Monad (EitherT e m) where
>   return = pure
>   (EitherT m) >>= f = EitherT $ do
>     v <- m
>     case v of
>       Left a -> return (Left a)
>       Right b -> runEitherT . f $ b
>
> swapEither :: (Functor m) => EitherT e m a -> EitherT a m e
> swapEither (EitherT a) = EitherT $ fmap f a
>   where f x = case x of
>                 Left a -> Right a
>                 Right a -> Left a
>
> eitherT :: Monad m =>
>            (a -> m c)
>         -> (b -> m c)
>         -> EitherT a m b
>         -> m c
> eitherT f g (EitherT m) = do
>   v <- m
>   either f g v

- ReaderT: monad transformer for Reader

> newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
>
> instance (Functor m) => Functor (ReaderT r m) where
>   fmap f (ReaderT g) = ReaderT $ fmap (fmap f) g
>
> instance (Applicative m) => Applicative (ReaderT r m) where
>   pure a = ReaderT . pure . pure $ a
>   (ReaderT f) <*> (ReaderT g) = ReaderT $ liftA2 (<*>) f g
>
> instance (Monad m) => Monad (ReaderT r m) where
>   return = pure
>   (ReaderT f) >>= g = ReaderT $ \r -> do
>     a <- f r
>     runReaderT (g a) r

- StateT : monad transformer for State

> newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
>
> instance (Functor m) => Functor (StateT s m) where
>   fmap f (StateT m) = StateT $ fmap (fmap g) m
>     where g (a, s) = (f a, s)
>
> instance (Monad m) => Applicative (StateT s m) where
>   pure a = StateT $ \s -> pure (a, s)
>   (StateT f) <*> (StateT a) = StateT $ \s -> do
>     (mf, s1) <- f s
>     (ma, s2) <- a s1
>     return (mf ma, s2)
>
> instance (Monad m) => Monad (StateT s m) where
>   return = pure
>   (StateT a) >>= f = StateT $ \s -> do
>     (ma, s1) <- a s
>     runStateT (f ma) s1
>
>
>
>
>
>

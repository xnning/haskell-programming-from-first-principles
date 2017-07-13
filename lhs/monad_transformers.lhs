Monad transformers
========================

Contents
------------------------

- work through more monad transformer types and instances
- look at the ordering and wrapping of monad transformer stacks
- lift, lift, lift, and lift some more

Extra
------------------------

> {-# LANGUAGE InstanceSigs #-}
> import Control.Applicative (liftA2)
> import Control.Monad (join, liftM)
> import Control.Monad.Trans.Except
> import Control.Monad.Identity

> newtype Reader r a =
>     Reader { runReader :: r -> a }
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
> instance Monad (Reader r) where
>   return = pure
>
>   (>>=) :: Reader r a
>         -> (a -> Reader r b)
>         -> Reader r b
>   (>>=) (Reader ra) f = Reader $ \r -> ((runReader . f) . ra) r r

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
> -- correspondence between StateT and Parser
>
> type Parser a = String -> Maybe (a, String)
> -- which is equivalent to
> type Parser' = StateT String Maybe

- Don't use `Writer` or `WriterT`: Writer can accumulate unevaluated thunks,
  causing memory leaks. It's also inappropriate for logging long-running or
  ongoing programs due to the fact that you can't retrieve any of the logged
  values until the computation is complete.

- Don't use `ListT`.

- If you have a transformer variant of a type and want to use it as if it was
  the non-transformer version, you need some structure that doesn’t really do
  anything: Identity.

> type Maybe' a = MaybeT Identity a
> type Either' e a = EitherT e Identity a
> type Reader' r a = ReaderT r Identity a
> type State' s a = StateT s Identity a

In general, don’t use hand-rolled versions of these transformer types without
good reason.

- Lexically inner is structurally outer: the additional structure m is always
  wrapped around our value. Also note it's only wrapped around things we can
  have, not things we need.

- MonadTrans is a typeclass with one core method: lift. lifting means you're
embedding an expression in a larger context by adding structure that doesn’t do
anything.

> class MonadTrans t where
>   -- | Lift a computation from the argument monad
>   --   to the constructed monad.
>   lift :: (Monad m) => m a -> t m a
>
> -- instances
>
> -- instance MonadTrans IdentityT where
> --   lift = IdentityT
>
> instance MonadTrans MaybeT where
>   lift = MaybeT. liftM Just
>
> instance MonadTrans (ReaderT r) where
>   lift = ReaderT . const

- The `MonadIO` class has `liftIO`, which keeps lifting your IO action until it
  is lifted over all structure embedded in the outermost IO type.

> class (Monad m) => MonadIO m where
> -- | Lift a computation from the 'IO' monad.
>   liftIO :: IO a -> m a
>
> -- examples
> -- instance (MonadIO m) => MonadIO (IdentityT m) where
> --   liftIO = IdentityT . liftIO
>
> instance (MonadIO m) => MonadIO (EitherT e m) where
>   liftIO = lift . liftIO

Exercises
------------------------

**Exercise: Wrap It Up**

> embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
> embedded =  MaybeT. ExceptT. ReaderT . (\x y -> return $ x y) $ (const (Right (Just 1)))

**Exercises: Lift More**

> instance MonadTrans (EitherT e) where
>   lift = EitherT . liftM Right
>
> instance MonadTrans (StateT s) where
>   lift m = StateT $ \s -> do
>     a <- m
>     return (a, s)

**Exercises: Some Instances**

> instance (MonadIO m) => MonadIO (MaybeT m) where
>   liftIO = lift . liftIO
>
> instance (MonadIO m) => MonadIO (ReaderT r m) where
>   liftIO = lift . liftIO
>
> instance (MonadIO m) => MonadIO (StateT s m) where
>   liftIO = lift . liftIO

Chapter Exercises
------------------------

**Write the code**

> rDec :: Num a => Reader a a
> rDec = Reader (subtract 1)
>
> rShow :: Show a => ReaderT a Identity String
> rShow = ReaderT (Identity . show)
>
> rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
> rPrintAndInc = ReaderT $ \a -> do
>   putStrLn $ "Hi: " ++ show a
>   return a
>
> sPrintIncAccum :: (Num a, Show a) => StateT a IO String
> sPrintIncAccum = StateT $ \a -> do
>   putStrLn $ "Hi: " ++ show a
>   return (show a, a + 1)

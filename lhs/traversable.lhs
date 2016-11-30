Foldable
------------------------

Contents
========================

- explain the Traversable typeclass and its canonical functions
- explore examples of Traversable in practical use
- tidy up some code using this typeclass
- and, of course, write some Traversable instances

Notes
========================

- Traversable depends on Applicative, and thus Functor, and is also superclassed by Foldable.

> class (Functor t, Foldable t) => Traversable' t where
>   {-# MINIMAL traverse' | sequenceA' #-}
>   traverse' :: Applicative f =>
>        (a -> f b)
>     -> t a
>     -> f (t b)
>   traverse' f = sequenceA' . fmap f
>
>   sequenceA' :: Applicative f => t (f a) -> f (t a)
>   sequenceA' = traverse' id

- Anytime you need to flip two type constructors around, or map something and then flip them around, that’s probably Traversable.

- Laws
    - Naturality: `t . traverse f = traverse (t . f)`
    - Identity: `traverse Identity = Identity`
    - Composition: `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`

Chapter Exercises
========================

> -- Identity
> newtype Identity a = Identity a deriving (Eq, Ord, Show)
>
> instance Functor Identity where
>   fmap f (Identity a) = Identity (f a)
>
> instance Foldable Identity where
>   foldMap f (Identity a) = f a
>
> instance Traversable Identity where
>   traverse f (Identity a) = Identity <$> f a
>
> -- Constant
>
> newtype Constant a b = Constant { getConstant :: a }
>
> instance Functor (Constant a) where
>   fmap f (Constant a) = Constant a
>
> instance Foldable (Constant a) where
>   foldMap f (Constant a) = mempty
>
> instance Traversable (Constant a) where
>   traverse f (Constant a) = pure (Constant a)
>
> -- Maybe
>
> data Optional a = Nada | Yep a
>
> instance Functor Optional where
>   fmap _ Nada = Nada
>   fmap f (Yep a) = Yep (f a)
>
> instance Foldable Optional where
>   foldMap f Nada = mempty
>   foldMap f (Yep a) = f a
>
> instance Traversable Optional where
>   traverse f Nada = pure Nada
>   traverse f (Yep a) = Yep <$> f a
>
> -- List
>
> data List a = Nil | Cons a (List a)
>
> instance Functor List where
>   fmap _ Nil = Nil
>   fmap f (Cons a as) = Cons (f a) (fmap f as)
>
> instance Foldable List where
>   foldMap f Nil = mempty
>   foldMap f (Cons a as) = mappend (f a) (foldMap f as)
>
> instance Traversable List where
>   traverse f Nil = pure Nil
>   traverse f (Cons a as) = Cons <$> (f a) <*> traverse f as
>
> -- Three
>
> data Three a b c = Three a b c
>
> instance Functor (Three a b) where
>   fmap f (Three a b c) = Three a b (f c)
>
> instance Foldable (Three a b) where
>   foldMap f (Three a b c) = f c
>
> instance Traversable (Three a b) where
>   traverse f (Three a b c) = Three a b <$> f c
>
> -- Three'
>
> data Three' a b = Three' a b b
>
> instance Functor (Three' a) where
>   fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)
>
> instance Foldable (Three' a) where
>   foldMap m (Three' a b1 b2) = m b1 `mappend` m b2
>
> instance Traversable (Three' a) where
>   traverse f (Three' a b1 b2) = Three' a <$> f b1 <*> f b2
>
> -- S
> data S n a = S (n a) a
> -- n : * -> *
> -- a : *
> -- S : (* -> *) -> * -> *
>
> instance Functor n => Functor (S n) where
>   fmap f (S na a) = S (fmap f na) (f a)
>
> instance Foldable (S n) where
>   foldMap f (S na a) = f a
>
> instance (Traversable n) => Traversable (S n) where
>   traverse f (S na a) = S <$> traverse f na  <*> f a
>
> -- Tree
>
> data Tree a =
>     Empty
>   | Leaf a
>   | Node (Tree a) a (Tree a)
>   deriving (Eq, Show)
>
> instance Functor Tree where
>   fmap _ Empty = Empty
>   fmap f (Leaf a) = Leaf (f a)
>   fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)
>
> instance Foldable Tree where
>   foldMap f Empty = mempty
>   foldMap f (Leaf a) = f a
>   foldMap f (Node l a r) = foldMap f l `mappend` f a `mappend` foldMap f r
>
> instance Traversable Tree where
>   traverse f Empty = pure Empty
>   traverse f (Leaf a) = Leaf <$> f a
>   traverse f (Node l a r) = Node <$> traverse f l <*> f a  <*> traverse f r

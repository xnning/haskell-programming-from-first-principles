Monoid, Semigroup
-----------------

Contents
========

-   Algebras
-   Laws
-   Monoids
-   Semigroups

Notes
=====

**Algebra**

-   in mathematic, algebra means the study of mathematical symbols and the rules governing their manipulation.

-   an algebra refers to some operations and the set they operate on.

-   in Haskell, these algebras can be implemented with typeclasses: the typeclasses define the set of operations; the instance defines how each operation will perform for a given type or set.

**Monoid**

-   A monoid is a binary associative operation with an identity.

-   in plain English, a monoid is a function that takes two arguments and follows two laws: associativity and identity.
    -   associativity: arguments can be regrouped in different orders and give the same result.
    -   identity: exists some value such that when we pass it as input to our function, the operation is rendered moot and the other value is returned.
-   in Haskell, Monoid is the typeclass that generalizes these laws across types.

``` sourceCode
-- class Monoid m where
--   mempty :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m
--   mconcat = foldr mappend mempty
```

-   Some types have more than one possible monoid, we usually enforce the unique instance rule by using newtype to separate the different monoidal behaviors.
    -   numbers: Sum, Product
    -   booleans: All, Any
    -   maybes: First, Last; reusing inhabited monoid.
-   Avoid orphan instances: put the intance in the typeclass file or the type file; if neither than use newtype to wrap original type

**Laws**

-   Laws circumscribe what constitutes a valid instance or concrete instance of the algebra or set of operations we’re working with.

-   Laws make up what algebras are. Algebras are defined by their laws and are useful principally for their laws.

-   Monoid instances must abide by the following laws:
    -   left identity: mappend mempty x = x
    -   right identity: mappend x mempty = x
    -   associativity: mappend x (mappend y z) = mappend (mappend x y) z
    -   mconcat = foldr mappend mempty

Extra
=====

``` sourceCode
import Data.Monoid
```

Exercises
=========

**Exercise: Optional Monoid**

``` sourceCode
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Only mempty
  mappend Nada e2 = e2
  mappend e1 Nada = e1
  mappend (Only e1) (Only e2) = Only (mappend e1 e2)
```

**Madness**

``` sourceCode
type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  mconcat [e, "! he said ",
           adv, " as he jumped into his car ",
           noun, " and drove off with this ",
           adj, " wife."]
```

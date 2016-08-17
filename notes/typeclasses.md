Typeclasses
-----------

-   Typeclasses and types in Haskell are, in a sense, opposites: where a declaration of a type defines how that type in particular is created, a declaration of a typeclass defines how a set of types are consumed or used in computations.

-   When you have a typeclass-constrained (ad-hoc) polymorphic value and need to evaluate it, the polymorphism must be resolved to a specific concrete type. But in some cases, particularly when you’re working in the GHCi REPL you will not have specified a concrete type for a polymorphic value. In those situations, the typeclass will default to a concrete type, and the default types are already set in the libraries.

-   The Haskell Report3 specifies the following defaults relevant to numerical computations:
    -   default Num Integer
    -   default Real Integer
    -   default Enum Integer
    -   default Integral Integer
    -   default Fractional Double
    -   default RealFrac Double
    -   default Floating Double
    -   default RealFloat Double
-   The use of polymorphic values without the ability to infer a specific type and no default rule will cause GHC to complain about an ambiguous type.

-   Haskell introduced and refined a means of writing ordinary programs that talk to the outside world without adding anything to the pure lambda calculus it is founded on. This property -- being lambda calculus and nothing more -- is what makes Haskell a purely functional programming language.

-   Typeclass instances we can magically derive are `Eq`, `Ord`, `Enum`, `Bounded`, `Read`, and `Show`, though there are some constraints on deriving some of these.

-   Read is a partial function, a function that doesn't return a proper value as a result for all possible inputs.

-   If we turn all warnings on with the `Wall` flag in our REPL or in our build configuration, then GHC will let us know when we’re not handling all cases: `:set -Wall`

Exercises
=========

**Multiple choice**

1.  c
2.  a, c
3.  a
4.  c
5.  a

**Does it typecheck?**

``` sourceCode
-- 1. No. No instance of Show.
data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2. No. No instance of Eq.
data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x

 -- 3.
 -- - Blah, Woot only.
 -- - Error. Number cannot be tested equal with Mood.
 -- - Error. No instance of Ord.

 -- 4. Yes.

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
              deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
```

**Given a datatype declaration, what can we do?**

``` sourceCode
data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- 1. No
phew = Papu (Rocks "chases") (Yeah True)

-- 2. Yes
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3. Yes
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4. No. No instance of Ord.
```

**Match the types**

1.  No. i should be instance of Num
2.  No. a should be instance of Fractional
3.  Yes.
4.  Yes.
5.  Yes.
6.  Yes.
7.  No. the return type should be Int.
8.  No. the return type should be Int.
9.  Yes.
10. Yes.
11. No. the type cannot be changed.

**Type-Kwon-Do Two: Electric Typealoo**

``` sourceCode
--1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

--2
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = fromInteger i + (f a)
```

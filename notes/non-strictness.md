Non-strictness
==============

Contents
--------

-   define call-by-name and call-by-need strategy
-   explain the main effects of nonstrict evaluation
-   live the Thunk Life
-   consider the runtime behavior of non-strict code in terms of sharing
-   develop methods for observing sharing and measuring program
    efficiency
-   bottom out with the bottoms

Notes
-----

-   Non-strictness is defined by the ability to evaluate expressions
    that have bindings which are bottom in them, as long as the bottom
    itself is never forced.

-   A truly lazy language memoizes, or holds in memory, the results of
    all the functions it does evaluate, and, outside of toy programs,
    this tends to use unacceptably large amounts of memory.
    Implementations of Haskell, such as GHC Haskell, are only obligated
    to be **non-strict** such that they have the same behavior with
    respect to bottom; they are not required to take a particular
    approach on how the program executes or how efficiently it does so.

-   What `seq` does is evaluate expressions up to *weak head normal
    form*. WHNF means it stops at the first data constructor or lambda.

``` haskell
 dc = (,) undefined undefined
 noDc = undefined
 lam = \_ -> undefined

 dc `seq` 1
 -- 1

 noDc `seq` 1
 -- Exception: Prelude.undefined

 lam `seq` 1
 -- 1
```

-   Case matching also chains evaluation. In haskell, case matching is
    strict, or at least the pattern matching of it is, up to WHNF. In
    Core, cases are always strict to WHNF. To see the difference,
    consider:

``` haskell
-- In haskell, evaluating f would be fine as GHC recognizes that we didn't really
-- use the case match for anything and drops the case expression entirely.
f = case undefined of {_ -> False}
-- However in GHC Core, the syntactically similar core expression will bottom out:
--   case undefined of {DEFAULT -> False}
```

-   A **thunk** is used to reference suspended computations that might
    be performed or computed at a later point in the program.
    -   Not all values get thunked. GHC will not thunk values which are
        merely data constructors. Data constructors are known to be
        constant, which justifies the safety of the optimization.
-   What promotes sharing: names.

-   What prevents sharing:
    1.  Inlining expressions where they get used prevents sharing
        because it creates independent thunks that will get computed
        separately.
    2.  Being a function with explicit, named arguments also prevents
        sharing.
    3.  Typeclass constraints also prevents sharing.
    4.  Implicit parameters.
    5.  Polymorphic expressions.
-   Refutable and irrefutable patterns
    -   An irrefutable pattern is one which will never fail to match.
    -   A refutable pattern is one which has potential failures.

``` haskell
-- pattern is refutable or not, not the functions.
refutable :: Bool -> Bool
refutable True = False
refutable False = True

irrefutable :: Bool -> Bool
irrefutable x = not x
```

-   Lazy patterns are also *irrefutable*.

``` haskell
strictPattern :: (a, b) -> String
strictPattern (a, b) = const "Cousin it" a

-- useful for unpacking datatype that might not get used
lazyPattern :: (a, b) -> String
lazyPattern ~(a, b) = const "Cousin it" a
```

-   Forcing evaluation: in some cases, it is cheaper to just compute
    something than to construct a thunk and then evaluate it later. This
    case is particularly common in numerics code when there are lots of
    Int and Double values around which are individually cheaper to
    conjure.
    -   `seq`

``` haskell
manualSeq :: Bool -> Int
manualSeq b = b `seq` 1
```

    + bang-patterns

``` haskell
banging :: Bool -> Int
banging !b = 1
```

    + Strict and StrictData pragmas.

Chapter Exercises
-----------------

**Strict List**

Takes forever to run without printing anything.

**What will :sprint output?**

``` haskell
let x = 1
x = _

let x = ['1']
x = "1"

let x = [1]
x = _

let x = 1 :: Int
x = 1

let f = \x -> x
let x = f 1
x = _

let f :: Int -> Int; f = \x -> x
let x = f 1
x = _
```

**Will printing this expression result in bottom?**

    snd (undefined, 1)
    NO.

    let x = undefined
    let y = x `seq` in snd (x, y)
    YES.

    length $ [1..5] ++ undefined
    YES.

    length $ [1..5] ++ [undefined]
    NO.

    const 1 undefined
    NO.

    const 1 (undefined `seq` 1)
    NO.

    const undefined 1
    YES.

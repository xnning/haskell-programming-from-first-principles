Basic libraries
========================

Contents
------------------------

- demonstrate how to measure the usage of time and space in your programs
- offer guidelines on when weak head normal form or normal form are appropriate when benchmarking code
- define constant applicative forms and explain argument saturation
- demonstrate and critically evaluate when to use different data structures in different circumstances
- sacrifice some jargons for the jargon gods

Notes
------------------------

- Benchmarking with `criterion`:

```haskell
module Main where

import Criterion.Main

infixl 9 !?
_      !? n | n < 0 = Nothing
[]     !? _         = Nothing
(x:_)  !? 0         = Just x
(_:xs) !? n         = xs !? (n - 1)

myList :: [Int]
myList = [1..9999]

main :: IO ()
main = defaultMain
  [ bench "index list 9999"
    $ whnf (myList !!) 9998
  , bench "index list maybe index 9999"
    $ whnf (myList !?) 9998
  ]
```

```haskell
defaultMain :: [Benchmark] -> IO ()

-- the key when determining whether you want whnf or nf is to think about what
-- you're trying to benchmark and if reaching the first data constructor will
-- actually do all the work you're trying to measure or not.
whnf :: (a -> b) -> a -> Benchmarkable
nf :: Control.DeepSeq.NFData b => (a -> b) -> a -> Benchmarkable
```

- Interestingly, a more efficient implementation of `!!`

```haskell
xs !! n
  | n < 0     = negIndex
  | otherwise =
      foldr (\x r k -> case k of
                         0 -> x
                         _ -> r (k - 1)) tooLarge xs n
```

- Profiling
    + `-prof` enables profiling. Used alone, it will require you to annotate "cost centers" manually.
    + `-fprof-auto` assigns all bindings not marked inline a cost center named after the binding.
    + `rtsopts` enables you to pass GHC RTS options to the generated library.
    This is optional so you can get a smaller binary if desired. We need this to
    tell our program to dump the profile to the .prof file named after our
    program.
    + `-O2` enables the highest level of program optimizations.

```bash
stack ghc -- -prof -fprof-auto -rtsopts -O2 tmp.hs

./tmp +RTS -P

cat tmp.prof

# profiling heap usage
./tmp +RTS -hc -p
hp2ps tmp.hp
```

- `CAF`: constant applicative forms. CAFs are expressions that have no free
  variables and are held in memory to be shared with all other expressions in a
  module. CAFs can make some programs faster since you don't have to keep
  re-evaluating shared values; but they can become memory-intensive quite
  quickly.
    + values
    + partially applied functions with named arguments
    + fully appied functions

- Use Map when you have keys and values instead of assciation lists. Using an
  Int as your key type is usually a sign you'd be better off with a HashMap,
  IntMap, or Vector.

- Updates (cons and append) to both ends of the data structure and concatenation
 are what Sequence is particularly known for.

- There are many variants of Vector. These include boxed, unboxed, immutable,
  mutable, and storable vectors. The default Vector type is implemented as a
  slice wrapper of Array. Thus slicing is quite cheap. You want a vector when
    + you need memory efficiency close to the theoretical maximum for the data
    you are working with
    + your data access is almost exclusively in terms of indexing via an Int
    value
    + you want uniform access times for accessing each element in the data
    structure
    + you will construct a Vector once and read it many time; or ou plan to use
    a mutable vector for efficient update



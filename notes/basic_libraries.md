# Basic libraries

## Contents

  - demonstrate how to measure the usage of time and space in your
    programs
  - offer guidelines on when weak head normal form or normal form are
    appropriate when benchmarking code
  - define constant applicative forms and explain argument saturation
  - demonstrate and critically evaluate when to use different data
    structures in different circumstances
  - sacrifice some jargons for the jargon gods

## Notes

  - Benchmarking with `criterion`:

<!-- end list -->

``` haskell
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

``` haskell
defaultMain :: [Benchmark] -> IO ()

-- the key when determining whether you want whnf or nf is to think about what
-- you're trying to benchmark and if reaching the first data constructor will
-- actually do all the work you're trying to measure or not.
whnf :: (a -> b) -> a -> Benchmarkable
nf :: Control.DeepSeq.NFData b => (a -> b) -> a -> Benchmarkable
```

  - Interestingly, a more efficient implementation of `!!`

<!-- end list -->

``` haskell
xs !! n
  | n < 0     = negIndex
  | otherwise =
      foldr (\x r k -> case k of
                         0 -> x
                         _ -> r (k - 1)) tooLarge xs n
```

  - Profiling
      - `-prof` enables profiling. Used alone, it will require you to
        annotate “cost centers” manually.
      - `-fprof-auto` assigns all bindings not marked inline a cost
        center named after the binding.
      - `rtsopts` enables you to pass GHC RTS options to the generated
        library. This is optional so you can get a smaller binary if
        desired. We need this to tell our program to dump the profile to
        the .prof file named a er our program.
      - `-O2` enables the highest level of program optimizations.

<!-- end list -->

``` bash
stack ghc -- -prof -fprof-auto -rtsopts -O2 tmp.hs

./tmp +RTS -P

cat tmp.prof

# profiling heap usage
./tmp +RTS -hc -p
hp2ps tmp.hp
```

  - `CAF`: constant applicative forms. CAFs are expressions that have no
    free variables and are held in memory to be shared with all other
    expressions in a module. CAFs can make some programs faster since
    you don’t have to keep re-evaluating shared values; but they can
    become memory-intensive quite quickly.
      - values
      - partially applied functions with named arguments
      - fully appied functions

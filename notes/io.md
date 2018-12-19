# IO

## Contents

  - explain how IO works operationally
  - explore what it should mean to you when you read a type that has IO
    in it
  - provide a bit more detail about the IO Functor, Applicative and
    Monad

## Notes

``` haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
```

  - IO primarily exists to give us a way to order operations and to
    disable some of the sharing. It only disables sharing for the
    terminal value it reduces to. Values that are not dependent on IO
    for their evaluation can still be shared.

  - Values of type IO a are not an a; they’re a description of how you
    might get an a. Having a recipe for a cake does not give you a cake.

  - Pure functional:
    
      - originally meant the semantics of the language would only be
        lambda calculus
      - Referential transparency: any function, when given the same
        inputs, returns the same result. an expression is referentially
        transparent when it can be replaced with its value without
        changing the behavior of a program.

  - A function that returns IO a is still referentially transparent, as
    it returns the same recipe for the same arguments.

<!-- end list -->

``` haskell
gimmeShelter :: Bool -> IO [Int]
gimmeShelter True  = replicateM 10 (randomRIO (0, 10))
gimmeShelter False = pure [0]
```

  - An unsafe means of enabling sharing for an IO action:
    `unsafePerformIO`.

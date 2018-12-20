# When things go wrong

## Contents

  - examine the Exception typeclass and methods
  - dip our toes into existential quantification
  - discuss ways of handling exceptions

## Notes

  - The `Exception` type class:

<!-- end list -->

``` haskell
class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  displayException :: e -> String

data SomeException where
  SomeException :: Exception e => e -> SomeException
```

  - The existential variable in `SomeException` allows us to raise error
    values of two totally different types without enumerating them in a
    sum type.

<!-- end list -->

``` haskell
data MyException =
  forall e .
  (Show e, Typeable e) => MyException e

multiError :: Int -> Either MyException Int
multiError n =
  case n of
    0 -> Left (MyException DivideByZero)
    1 -> Left (MyException StackOverflow)
    2 -> Right n
```

  - `Typeable`: exists to permit types to be known at runtime, allowing
    for a sort of dynamic typechecking.

  - Typeable has one method. We don’t usually call this function
    directly. But it gets called for us by the `fromException` function,
    and `fromException` is called by the `catch` function.

<!-- end list -->

``` haskell
cast :: (Typeable a, Typeable b) => a -> Maybe b
```

  - `catch` runs only if the exception matching the type you specified
    gets thrown, and it gives you an opportunitity to recover from the
    error and still satisfy the original type.

<!-- end list -->

``` haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

  - `try` can turn implicit exceptions into an explicit Either.

<!-- end list -->

``` haskell
try :: Exception e => IO a -> IO (Either e a)
```

  - `throwIO` throws an exception.

<!-- end list -->

``` haskell
throwIO :: Exception e => e -> IO a
```

  - Write an exception

<!-- end list -->

    data NotDivThree = NotDivThree deriving (Eq, Show)
    
    instance Exception NotDivThree

  - The exception handling mechanism is not for, nor should be used for,
    catching bottoms.

# Basic datatypes

Numeric types
  - Integral numbers
    - Int: fixed-precision; an artifact of what computer hardware has supported natively over the years.
    - Integer: arbitrarily large (or small) numbers,
  - Fractional
    - Float: single-precision floating point numbers.
      Violate some common assumptions and should only be used with great care.
    - Double: double-precision floating point numbers.
    - Rational: fractional number that represents a ratio of two integers; arbitrarily precise but not as efficient as Scientific.
    - Scientific: a space efficient and almost-arbitrary precision scientific number type; represented using scientific notation, with coefficient as an Integer and the exponent as an Int.

values of Fractional a => a default to the floating point type Double.

six categories of entities that have names.
  - term-level: variables, data constructors
  - type-level: type variables, type constructors, typeclasses
  - modules.

## Exercises

1. `length :: Foldable t => t a -> Int`
2. 5; 3; 3; 4
3. `6 / length [1, 2, 3]`
4. should use `div`
5. Bool; True
6. Bool; False
7. True; not work; 5; False; not work
8.
``` haskell
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x
```
9.
``` haskell
myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else -x
```
10.
``` haskell
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
let f x y = ((snd x, snd y), (fst x, fst y))
```

### Correcting syntax

1.
``` haskell
x = (+)
f xs = w `x` 1
where w = length xs
```
2.
``` haskell
\x=x
```
3.
``` haskell
\(x : xs) -> x
```
4.
``` haskell
f (a, b) = a
```

### Match the function names to their types

1. c
2. b
3. a
4. d

Types
===============

Type signatures may have three kinds of types:
- concrete
- constrained polymorphic: also called ad-hoc polymorphism. Implemented with typeclasses; decreasing the number of concrete types it could be, but increasing what you can actually do with it by defining and bringing into scope a set of operations.
- parametrically polymorphic: refer to type variables, or parameters. Parametricity means that the behavior of a function with respect to the types of its (parametrically polymorphic) arguments is uniform. The behavior can not change just because it was applied to an argument of a different type.

Exercise
--------------

**Multiple choice**

1. c
2. a
3. c
4. c

**Determine the type**

1. a - f
   - 54 :: (Num a) => a
   - (0, "doge") :: (Num a) => (a, String)
   - (0, "doge") :: (Integer, String)
   - False :: Bool
   - 5 :: Int
   - False :: Bool
2. Num a => a
3. Num a => a -> a
4. Functional a => a
5. String

**Does it compile**

```
1.
bigNum = (^) 5
wahoo = bigNum $ 10

2.

x = print
y = print "woohoo!"
z = x "hello world"

3.

a = (+)
b = 5
c = a 10
d = c 200

4.

b = 10000 * c
a = 12 + b
```

**Type variable or specific type constructor?**

2. fully: zed
concrete: Zed, Blah

3. fully: a
constrained: b
concrete: C

4. fully: f, g
concrete: C

**Write a type signature**

1. a -> a
2. Ord a => a -> Bool
3. (a, b) -> b

**Given a type, write the function**

1. i x = x
2. c x y = x
3. c'' x y = x
4. c' x y = y
5. r l = tail l
6. a f b = b
7. a' f b = f b

**Fix it**

1.

``` haskell
module sing where
     fstString :: [Char] -> [Char]
     fstString x = x ++ " in the rain"

     sndString :: [Char] -> [Char]
     sndString x = x ++ " over the rainbow"

     sing = if (x > y) then fstString x else sndString y
         where x = "Singin"
               y = "Somewhere"
```

2.

x < y

3.

``` haskell
main :: IO ()
main = do
  print (1 + 2)
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
 where blah = negate 1
```

**Type-Kwon-Do**

1. h x = g (f x)
2. e x = w (q x)
3. xform (x, y) = (xz x, yz y)
4. munge f y x = a where (a, b) = g (f x)

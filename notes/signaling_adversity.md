Signaling adversity
-------------------

Contents
========

-   `Nothing`, or `Just` `Maybe`.
-   `Either` left or right, but not both
-   higher-kindedness
-   anamorphisms, but not animorphs

Notes
=====

-   In `Either`, `Left` is used as invalid or error constructor.

-   `*`:
    -   kind of all standard lifted types.
    -   a lifted type is any that can be inhabited by bottom.
    -   represented by a pointer
-   `#`:
    -   kind of unlifted types.
    -   unlifted types are any type which cannot be inhabited by bottom.
    -   native machine types and raw pointers.
-   Newtypes are a special case in that they are kind \*, but are unlifted because their representation is identical to that of the type they contain, so the newtype itself is not creating any new pointer beyond that of the type it contains.

Extra
=====

``` haskell
import Data.Maybe (fromMaybe, fromJust)
```

Exercises
=========

**Determine the kinds**

1.  \*
2.  \*; \* -&gt; \*

**String processing**

``` haskell
-- 1
notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

-- 2
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = f . words
    where f (h:t@((a:_):_))
              | h == "the" && a `elem` "aeiou" = 1 + f t
              | otherwise = f t
          f _ = 0

-- 3
countVowels :: String -> Integer
countVowels = toInteger . length . filter (`elem` "aeiou")
```

**Validate the word**

``` haskell
newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = if cs > vs then Nothing else Just (Word' str)
    where vs = length . filter (`elem` vowels) $ str
          cs = length str - vs
```

**It’s only Natural**

``` haskell
data Nat =
     Zero
   | Succ Nat
   deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

integerToNat :: Integer -> Maybe Nat
integerToNat i | i == 0 = Just Zero
               | i > 0 = Just . Succ . fromJust . integerToNat $ i - 1
               | otherwise = Nothing
```

**Small library for Maybe**

``` haskell
-- 1
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

-- 3
fromMaybe' :: a -> Maybe a -> a
fromMaybe' a Nothing = a
fromMaybe' _ (Just a) = a

-- 4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (h:t) = Just h

-- 5
catMaybes :: [Maybe a] -> [a]
catMaybes = map (\(Just n) -> n) . filter isJust

-- 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
    where f Nothing _ = Nothing
          f _ Nothing = Nothing
          f (Just a) (Just as) = Just (a : as)
```

**Small library for Either**

``` haskell
-- 1
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
    where f (Left a) as = a : as
          f (Right _) as = as

-- 2
rights' :: [Either a b] -> [b]
rights' = foldr f []
    where f (Left _) as = as
          f (Right a) as = a : as

-- 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

-- 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just . f $ b

-- 5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

-- 6
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
```

**Unfolds**

``` haskell
-- 1
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- 2
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
    case f b of
      Just (a, b2) -> a : myUnfoldr f b2
      Nothing -> []

-- 3
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, f a)) x
```

**Finally something other than a list!**

``` haskell
data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- 1
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a =
    case f a of
       Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)
       Nothing -> Leaf

-- 2
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
    where f x | x >= n = Nothing
              | otherwise = Just (x + 1, x, x + 1)
```

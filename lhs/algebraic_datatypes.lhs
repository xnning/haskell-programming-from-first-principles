Algebraic datatypes
-------------------------

- Algebraic datatypes in Haskell are algebraic because we can describe the patterns of argument structures using two basic operations: sum and product.

- cardinality
    - Nullary constructors represent one value
    - Unary constructor always have the same cardinality as the type they contain.

- `newtype`: define a type that can only ever have a single unary data constructor.
    - A advantages over a vanilla data declaration: no runtime overhead, as it reuses the representation of the type it contains. The difference between newtype and the type it contains is gone by the time the compiler generates the code.
    - One key contrast between a newtype and a type alias is that you can define typeclass instances for newtypes that differ from the instances for their underlying type.

- Language pragmas, also called extensions, are special instructions to the compiler. They tell the compiler to process input in ways beyond what the standard provides for.

- A language pragma `GeneralizedNewtypeDeriving` allow our newtype to rely on a typeclass instance for the type it contains.

Extra
=========================

> {-# LANGUAGE FlexibleInstances #-}

Exericses
=========================

**Exercises: Dog Types**

1. type constructor
2. `* -> *`
3. `*`
4. Num a => Doggies a
5. Doggies Integer
6. Doggies String
7. It is a type constructor if appear in type level, and is a data constructor if appear in term level.
8. doge -> DogueDeBordeaux doge
9. DogueDeBordeaux String

**Exercises: Vehicles**

> data Price = Price Integer deriving (Eq, Show)
> data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
> data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
>
> data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

> -- 1. Vehicle
> -- 2
> isCar :: Vehicle -> Bool
> isCar (Car _ _) = True
> isCar _ = False
>
> isPlane :: Vehicle -> Bool
> isPlane (Plane _) = True
> isPlane _ = False
>
> areCars :: [Vehicle] -> [Bool]
> areCars = map isCar
>
> -- 3
> getManu :: Vehicle -> Manufacturer
> getManu (Car m _) = m
>
> -- 4. No exhaustive cases error.
>
> -- 5. data Vehicle = Car Manufacturer Price | Plane Airline Integer

**Exercises: Cardinality**

1. 1
2. 3
3. 65536
4. Int has very large bound; integer has no bounds.
5. 2^8

**Exercises: For Example**

1. Example; it can only be MakeExample
2. It gives the definition and also the typeclass instances.
3. MakeExample :: Int -> Example

**Exercises: Logic Goats**

> class TooMany a where
>    tooMany :: a -> Bool
>
> instance TooMany Int where
>    tooMany n = n > 42
>
> instance TooMany (Int, String) where
>    tooMany (i, _) = tooMany i
> instance TooMany (Int, Int) where
>    tooMany (i1, i2) = tooMany (i1 + i2)
> instance (Num a, TooMany a) => TooMany (a, a) where
>    tooMany (i1, i2) = tooMany (i1 + i2)

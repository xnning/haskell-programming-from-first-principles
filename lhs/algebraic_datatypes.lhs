Algebraic datatypes
========================

- Algebraic datatypes in Haskell are algebraic because we can describe the patterns of argument structures using two basic operations: sum and product.

- cardinality
    - Nullary constructors represent one value
    - Unary constructor always have the same cardinality as the type they contain.
    - Sum types are + or addition.
    - Product types are product or multiplication.
    - Function type is the exponent operator.

- `newtype`: define a type that can only ever have a single unary data constructor.
    - A advantages over a vanilla data declaration: no runtime overhead, as it reuses the representation of the type it contains. The difference between newtype and the type it contains is gone by the time the compiler generates the code.
    - One key contrast between a newtype and a type alias is that you can define typeclass instances for newtypes that differ from the instances for their underlying type.

- Language pragmas, also called extensions, are special instructions to the compiler. They tell the compiler to process input in ways beyond what the standard provides for.

- A language pragma `GeneralizedNewtypeDeriving` allow our newtype to rely on a typeclass instance for the type it contains.

- Whenever we have a product that uses record accessors, keep it separate of any sum type that is wrapping it. To do this, split out the product into an independent type with its own type constructor.

Extra
-------------------------

> {-# LANGUAGE FlexibleInstances #-}

> import Data.List (sortBy, groupBy, findIndex, intercalate, elemIndex, sort, group, maximumBy)
> import Data.Char
> import Data.Ord (comparing)

Exericses
-------------------------

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

**Exercises: Pity the Bool**

1. 4
2. 258; out of range error

**Exercises: Jammin**

> data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Show, Ord)
> -- 2
> data JamJars = Jam { fruit :: Fruit, jars :: Int} deriving (Eq, Show)
> -- 3. 4 * num of Int
>
> row1 = Jam {fruit = Peach, jars = 5}
> row2 = Jam {fruit = Peach, jars = 3}
> row3 = Jam {fruit = Plum, jars = 8}
> row4 = Jam {fruit = Plum, jars = 4}
> row5 = Jam {fruit = Apple, jars = 10}
> row6 = Jam {fruit = Blackberry, jars = 7}
> row7 = Jam {fruit = Blackberry, jars = 4}
> allJam = [row1, row2, row3, row4, row5, row6, row7]
> -- 5
> mapJars :: [JamJars] -> [Int]
> mapJars = map jars
>
> -- 6
> sumJars :: [JamJars] -> Int
> sumJars = sum . (map jars)
>
> -- 7
> maxJars :: [JamJars] -> JamJars
> maxJars [] = undefined
> maxJars js = foldr (\a b -> if jars a > jars b then a else b) (head js) js
>
> -- 9
>
> compareKind (Jam k _) (Jam k' _) = compare k k'
> sortJars :: [JamJars] -> [JamJars]
> sortJars = sortBy compareKind
>
> -- 10
> groupJars :: [JamJars] -> [[JamJars]]
> groupJars = groupBy (\a b -> fruit a == fruit b) . sortJars

**Exercises: How Does Your Garden Grow?**

> type Gardener = String
> data Garden =
>    Gardenia Gardener
>  | Daisy Gardener
>  | Rose Gardener
>  | Lilac Gardener deriving (Show)

**Exercise: Programmers**

> data OperatingSystem = GnuPlusLinux
>    | OpenBSDPlusNevermindJustBSDStill | Mac
>    | Windows deriving (Eq, Show)
>
> data ProgrammingLanguage =  Haskell | Agda | Idris | PureScript deriving (Eq, Show)
> data Programmer =
>   Programmer { os :: OperatingSystem
>              , lang :: ProgrammingLanguage } deriving (Eq, Show)
>
> allOperatingSystems :: [OperatingSystem]
> allOperatingSystems =
>     [ GnuPlusLinux
>     , OpenBSDPlusNevermindJustBSDStill , Mac
>     , Windows
>     ]
>
> allLanguages :: [ProgrammingLanguage]
> allLanguages = [Haskell, Agda, Idris, PureScript]
>
> allProgrammers :: [Programmer]
> allProgrammers = [Programmer os lang | os <- allOperatingSystems
>                                      , lang <- allLanguages]

**Exponentiation in what order?**

Yes.

**Exercises: The Quad**

1. 4 + 4 = 8
2. 4 * 4 = 16
3. 4 ^ 4 = 256
4. 2 * 2 * 2 = 8
5. 2 ^ 2 ^ 2 = 16
6. 2 ^ 4 ^ 4 = 16384

**Binary Tree**

> data BinaryTree a =
>       Leaf
>     | Node (BinaryTree a) a (BinaryTree a)
>     deriving (Eq, Ord, Show)
>
> mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
> mapTree _ Leaf = Leaf
> mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)
>
> preorder :: BinaryTree a -> [a]
> preorder Leaf = []
> preorder (Node left a right) = a : preorder left ++ preorder right
>
> inorder :: BinaryTree a -> [a]
> inorder Leaf = []
> inorder (Node left a right) = inorder left ++ [a] ++ inorder right
>
> postorder :: BinaryTree a -> [a]
> postorder Leaf = []
> postorder (Node left a right) = postorder left ++ postorder right ++ [a]
>
> foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
> foldTree _ v Leaf = v
> foldTree f v (Node left a right) = foldTree f (f a (foldTree f v left)) right

Chapter Exercises
------------------------

**Multiple choice**

1. a
2. c
3. b
4. c

**Ciphers**

> vigenereChar num char = chr (ord 'a' + shift)
>    where distance = ord char - ord 'a'
>          shift = (distance + num) `mod` 26
>
> -- word for single word
> vigenere str key = zipWith vigenereChar (map num (cycle key)) str
>    where num k = ord k - ord 'a'
>
> unVigenere str key = zipWith vigenereChar (map num (cycle key)) str
>    where num k = 26 - (ord k - ord 'a')

**As-patterns**

> -- 1
> isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
> isSubsequenceOf [] _ = True
> isSubsequenceOf (h1:_) [] = False
> isSubsequenceOf l1@(h1:t1) (h2:t2) =
>     if h1 == h2 then isSubsequenceOf t1 t2 else isSubsequenceOf l1 t2
>
> -- 2
> capitalizeWords :: String -> [(String, String)]
> capitalizeWords = map f . words
>     where f s@(h:t) = (s, toUpper h : t)

**Language exercises**

> -- 1
> capitalizeWord :: String -> String
> capitalizeWord [] = []
> capitalizeWord (h:t) = toUpper h : t
>
> -- 2
> capitalizeParagraph :: String -> String
> capitalizeParagraph sen = intercalate " " . map f $ sens
>     where sens = splitSentences sen
>           f = unwords . (\(h:t) -> capitalizeWord h : t) . words
>
> splitSentences :: String -> [String]
> splitSentences [] = []
> splitSentences str =
>   case findIndex (== '.') str of
>     Just num -> let (fst, snd) = splitAt (num + 1) str
>                 in fst : splitSentences (dropWhile (== ' ') snd)
>     Nothing -> [str]

**Phone exercise**

>
> data DaPhone = DaPhone [(Char, String)]
>
> -- validButtons = "1234567890*#"
> type Digit = Char
>
> -- Valid presses: 1 and up
> type Presses = Int
>
> reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
> reverseTaps phone c =
>     if isUpper c
>     then [('*', 1), tap phone (toLower c)]
>     else [tap phone c]
>
> tap :: DaPhone -> Char -> (Digit, Presses)
> tap (DaPhone ((digit, press):tl)) c =
>     case elemIndex c press of
>       Just idx -> (digit, idx + 1)
>       Nothing -> tap (DaPhone tl) c
>
> cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
> cellPhonesDead phone = foldMap (reverseTaps phone)
>
> -- 3
> fingerTaps :: [(Digit, Presses)] -> Presses
> fingerTaps = sum . map snd
>
> -- 4
> mostPopularLetter :: String -> Char
> mostPopularLetter = head . maximumBy (comparing length) . group . sort
>
> mostPopularLetterCost :: DaPhone -> String -> Presses
> mostPopularLetterCost phone s = (fingerTaps cost) * len
>     where c = maximumBy (comparing length) . group . sort $ s
>           len = length c
>           cost = reverseTaps phone (head c)
>
> -- 5
> coolestLtr :: [String] -> Char
> coolestLtr = mostPopularLetter . concat
>
> coolestWord :: [String] -> String
> coolestWord = head . maximumBy (comparing length) . group . sort . (foldMap words)
>
> phone = DaPhone [ ('0', "0"), ('1', "1")
>                 , ('2', "abc2"), ('3', "def3")
>                 , ('4', "ghi4"), ('5', "jkl5")
>                 , ('6', "mno6"), ('7', "pqrs7")
>                 , ('8', "tuv8"), ('9', "wxyz9")
>                 ]

**Hutton’s Razor**

> data Expr
>     = Lit Integer
>     | Add Expr Expr
>
> eval :: Expr -> Integer
> eval (Lit n) = n
> eval (Add e1 e2) = (eval e1) + (eval e2)
>
> printExpr :: Expr -> String
> printExpr (Lit n) = show n
> printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2

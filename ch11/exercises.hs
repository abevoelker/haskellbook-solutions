-- 11.5

-- Exercises: Dog Types

-- 1) Type constructor

-- 2) Doggies :: * -> *

-- 3) Doggies String :: *

-- 4) Husky 10 :: Num a => Doggies a

-- 5) Husky (10 :: Integer) :: Doggies Integer

-- 6) Mastiff "Scooby Doo" :: Doggies String

-- 7) Both - the name is used for both purposes. GHC will understand which is
--    which based on context.

-- 8) DogueDeBordeaux :: doge -> DogueDeBordeaux doge

-- 9) DogueDeBordeaux "doggie!" :: DogueDeBordeaux String


-- 11.6

-- Exercises: Vehicles

-- 1)
myCar :: Vehicle

-- 2)
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3)
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = error "only cars have manufacturers"

-- 4) It will raise a runtime error (bottom). The return type should
--    be changed to Maybe Manufacturer so we can return Nothing instead.

-- 5) Updated types and functions:

data Size = Length Integer
          deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False


-- 11.8

-- Exercises: Cardinality

-- 1) 1

-- 2) 3

-- 3) 65536

-- 4) (maxBound :: Int) is 9223372036854775807 and (minBound :: Int) is
--    -9223372036854775808 on my machine. Those values correspond to the max and
--    min values of a 64-bit signed integer, so I believe the cardinality is
--    2^64 for Int. (maxBound :: Integer) errors out, and `:i Integer` shows
--    that it doesn't implement the Bounded typeclass, so so I'm thinking that
--    Integer's cardinality is infinite.

-- 5) 8 corresponds to a number of bits. A bit can hold 2 values, and when you
--    string 8 of them together, that allows 2^8, i.e. 256, different possible
--    combinations (values).


-- Exercises: For Example

-- 1)
-- Q: What is the type of data constructor MakeExample?
-- A: MakeExample :: Example
-- Q: What happens when you request the type of Example?
-- A: It errors out with "Not in scope: data constructor ‘Example’" because
--    Example is a type constructor

-- 2)
-- Q: What if you try :info on Example in GHCi?
-- A: It works!
-- Q: Can you determine what typeclass instances are defined for the Example
--    type using :info in GHCi?
-- A: Yep, it shows the Show typeclass instance being defined:
--    > :i Example
--    data Example = MakeExample      -- Defined at <interactive>:49:1
--    instance Show Example -- Defined at <interactive>:49:37

-- 3)
data Foo = MakeExample Int deriving Show
-- Q: What has changed when you query MakeExample with :type in GHCi?
-- A: It shows the updated signature reflecting it being a unary data
--    constructor:
-- > :t MakeExample
-- MakeExample :: Int -> Foo
--
-- However this only works because GHCI lets you reassign things. If you
-- put both data declarations into the same file, you get an error:
--
--     Multiple declarations of ‘MakeExample’
--     Declared at: /tmp/foo.hs:1:16
--                  /tmp/foo.hs:3:12
-- Failed, modules loaded: none.


-- 11.9

-- Exercises: Logic Goats

-- 1)
{-# LANGUAGE FlexibleInstances #-}
instance TooMany (Int, String) where
  tooMany x = (fst x) > 42

-- 2)
{-# LANGUAGE FlexibleInstances #-}
instance TooMany (Int, Int) where
  tooMany x = (fst x) + (snd x) > 42

-- 3)
{-# LANGUAGE FlexibleInstances #-}
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany x = tooMany (fst x) || tooMany (snd x)


-- 11.10

-- Exercises: Pity the Bool

-- 1)   Big Bool | Small Bool
--    = Big (2)  + Small (2)
--    Big and Small are unary data constructors, so they have the cardinality
--    of the type they contain, which for Bool is 2
--    = 2 + 2
--    = 4

-- 2)
-- Q: What is the cardinality of NumberOrBool?
-- A:   Numba Int8 | BoolyBool Bool
--    = Numba (256) + BoolyBool (2)
--    = 256 + 2
--    = 258
-- Q: What happens if you try to create a Numba with a numeric literal larger
--    than 127?
-- A: It gives a warning that "Literal 129 is out of the Int8 range -128..127"
--    however it still creates it, but the Int8 value has overflowed the 8 bits
--    so that the actual value is less than what you requested.
-- Q: And with a numeric literal smaller than (-128)?
-- A: Same warning as before, except the value underflows the 8-bits so you end
--    up with a value that is more than you requested.


-- 11.12

-- Exercises: How Does Your Garden Grow?

-- 1)
data Garden =
  Gardenia Gardener
| Daisy Gardener
| Rose Gardener
| Lilac Gardener
deriving Show


-- 11.13

-- Exercise: Programmers

allProgrammers :: [Programmer]
allProgrammers = [ Programmer{lang = x, os = y} | x <- allLanguages, y <- allOperatingSystems ]


-- 11.14

-- Exponentiation in what order?

-- Yes, it holds.
-- Note: this can help list the possible return value enumerations:
-- [(x, y, z) | x <- [True, False], y <- [True, False], z <- [True, False]]

convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = True
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False

-- Exercises: The Quad

-- 1) Quad has 4 inhabitants. eQuad can take on 8 different forms.

-- 2) 16

-- 3) 256

-- 4) 8

-- 5) 16

-- 6) 65536 because (c ^ b) ^ a = c ^ (b * a) = 4 ^ (4 * 2) = 65536


-- 11.17

-- Write map for BinaryTree

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

-- Convert binary trees to lists

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (preorder left) ++ [a] ++ (preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (preorder left) ++ (preorder right) ++ [a]

-- Write foldr for BinaryTree
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b tree = foldr f b (inorder tree)


-- 11.18

-- Multiple choice

-- 1) a

-- 2) c

-- 3) b

-- 4) c

-- Ciphers

-- See vigenere.hs
-- encode "MEET AT DAWN" "ALLY" = "MPPR AE OYWY"
-- decode "MPPR AE OYWY" "ALLY" = "MEET AT DAWN"

-- As-patterns

-- 1)
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _  = True
isSubseqOf _  [] = False
isSubseqOf xs'@(x:xs) (y:ys)
  | x == y    = isSubseqOf xs  ys
  | otherwise = isSubseqOf xs' ys

-- 2)
import Data.Char

capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords x  = concat $ map capitalizeWords' (words x) where
  capitalizeWords' :: String -> [(String, String)]
  capitalizeWords' [] = []
  capitalizeWords' orig@(x:xs) = [(toUpper x : xs, orig)]

-- Language exercises

-- 1)
import Data.Char
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

-- 2)
capitalizeParagraph :: String -> String
capitalizeParagraph x = unwords $ go (words x) True where
  go :: [String] -> Bool -> [String]
  go [] _ = []
  go (x:xs) capWord
    | capWord   = (capitalizeWord x) : (go xs capNextWord)
    | otherwise = x : (go xs capNextWord)
    where
      capNextWord = (last x == '.')

-- Phone exercise

-- 1) See phone.hs

-- 2)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead x y = concat $ map (reverseTaps x) y

-- 3)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps x = sum (map snd x)

-- 4)

mostPopularLetter :: String -> Char
mostPopularLetter x = fst (maximumBy (comparing snd) letterCounts) where
  letterCounts = map (\x' -> (head x', length x')) $ group $ sort x

mostPopularLetterCosts = map (\x -> (x, (fingerTaps $ (reverseTaps defaultPhone x)))) (map mostPopularLetter convo)

-- 5)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

-- > coolestLtr convo
-- ' '

coolestWord :: [String] -> String
coolestWord x = fst (maximumBy (comparing snd) wordCounts) where
  allWords = concat (map words x)
  wordCounts = map (\x' -> (head x', length x')) $ group $ sort allWords

-- > coolestWord convo
-- "Lol"


-- DEPRECATED
-- These exercises appeared in previous versions of the book, but no longer
-- seem to be in the latest version.

-- 11.9

-- 1) Done - see jammin.hs

-- 2)
data JamJars =
  Jam { fruit :: Fruit
      , count :: Int }
      deriving (Eq, Show)

-- 3)  Jam Fruit Int
--   =     (4) * (2^64)
--   = 73786976294838206464

-- 4) (okay)

-- 5)
row1 = Jam Plum 8
row2 = Jam Blackberry 5
row3 = Jam Apple 3
row4 = Jam Peach 10
row5 = Jam Apple 9
row6 = Jam Peach 3
allJam = [row1, row2, row3, row4, row5, row6]

-- 6)
totalJars :: [JamJars] -> Int
totalJars = sum . (map count)
-- or
totalJars' :: [JamJars] -> Int
totalJars' = foldr (\a b -> count a + b) 0

-- 7)
import Data.List
mostRow :: [JamJars] -> JamJars
mostRow xs = maximumBy (\x y -> compare (count x) (count y)) xs

-- 8) okay:
-- > import Data.List
-- > :t sortBy
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- > :t groupBy
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]

-- 9)
-- > sortBy compareKind allJam
-- [Jam {fruit = Peach, count = 10},Jam {fruit = Peach, count = 3},Jam {fruit = Plum, count = 8},Jam {fruit = Apple, count = 3},Jam {fruit = Apple, count = 9},Jam {fruit = Blackberry, count = 5}]

-- 10)
compareCount :: JamJars -> JamJars -> Ordering
compareCount (Jam _ c) (Jam _ c') = compare c' c

sameKind :: JamJars -> JamJars -> Bool
sameKind (Jam k _) (Jam k' _) = k == k'

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy sameKind . (sortBy compareKind) . (sortBy compareCount)

-- *Jammin> groupJam allJam
-- [[Jam {fruit = Peach, count = 10},Jam {fruit = Peach, count = 3}],[Jam {fruit = Plum, count = 8}],[Jam {fruit = Apple, count = 9},Jam {fruit = Apple, count = 3}],[Jam {fruit = Blackberry, count = 5}]]

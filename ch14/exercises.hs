-- 14.3

-- Intermission: Short Exercise

-- see ShortExercise.hs


-- 14.7

-- Validating numbers into words

-- see WordNumberTest.hs

-- Using QuickCheck

-- 1)
import Test.QuickCheck

half x = x / 2
halfIdentity = (*2) . half

prop_half :: Double -> Bool
prop_half x = x == halfIdentity x

main :: IO ()
main = quickCheck prop_half

-- 2)
import Data.List (sort)
import Test.QuickCheck

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_sort :: (Ord a) => [a] -> Bool
prop_sort xs = listOrdered (sort xs)

main :: IO ()
main = quickCheck (prop_sort :: [Int] -> Bool)

-- 3)
import Test.QuickCheck

plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative x y =
  x + y == y + x

prop_plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_plusAssociative x y z = plusAssociative x y z

prop_plusCommutative :: (Eq a, Num a) => a -> a -> Bool
prop_plusCommutative x y = plusCommutative x y

main :: IO ()
main = do
  quickCheck (prop_plusAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (prop_plusAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (prop_plusCommutative :: Int -> Int -> Bool)
  quickCheck (prop_plusCommutative :: Integer -> Integer -> Bool)

-- 4)
import Test.QuickCheck

multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative x y =
  x * y == y * x

prop_multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_multAssociative x y z = multAssociative x y z

prop_multCommutative :: (Eq a, Num a) => a -> a -> Bool
prop_multCommutative x y = multCommutative x y

main :: IO ()
main = do
  quickCheck (prop_multAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (prop_multAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (prop_multCommutative :: Int -> Int -> Bool)
  quickCheck (prop_multCommutative :: Integer -> Integer -> Bool)

-- 5)
import Test.QuickCheck

prop_quotRem :: Integral a => a -> a -> Bool
prop_quotRem x y = (quot x y)*y + (rem x y) == x

prop_divMod :: Integral a => a -> a -> Bool
prop_divMod x y = (div x y)*y + (mod x y) == x

genNonZero :: Gen Integer
genNonZero = (arbitrary :: Gen Integer) `suchThat` (/= 0)

genTuple :: Gen (Integer, Integer)
genTuple = do
  a <- (arbitrary :: Gen Integer)
  b <- (arbitrary :: Gen Integer) `suchThat` (/= 0)
  return (a,b)

main :: IO ()
main = do
  quickCheck $ forAll genTuple (\(x,y) -> prop_quotRem x y)
  quickCheck $ forAll genTuple (\(x,y) -> prop_divMod  x y)

-- 6)
-- (^) appears to be neither associative nor commutative,
-- as all tests failed (associativity falsified with 0 0 0,
-- commutativity falsified with 0 1)
import Test.QuickCheck

expAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

expCommutative x y =
  x ^ y == y ^ x

prop_expAssociative :: (Eq a, Integral a, Num a) => a -> a -> a -> Bool
prop_expAssociative x y z = expAssociative x y z

prop_expCommutative :: (Eq a, Integral a, Num a) => a -> a -> Bool
prop_expCommutative x y = expCommutative x y

main :: IO ()
main = do
  quickCheck (prop_expAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (prop_expAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (prop_expCommutative :: Int -> Int -> Bool)
  quickCheck (prop_expCommutative :: Integer -> Integer -> Bool)

-- 7)
-- All tests pass
import Test.QuickCheck

prop_reverseTwiceId :: (Eq a) => [a] -> Bool
prop_reverseTwiceId x = (reverse . reverse) x == x

main :: IO ()
main = do
  quickCheck (prop_reverseTwiceId :: [Integer] -> Bool)
  quickCheck (prop_reverseTwiceId :: [Char]    -> Bool)

-- 8)
-- All tests pass
import Data.List
import Test.QuickCheck

-- This SO answer was helpful for puzzling this one out:
-- https://stackoverflow.com/a/2017664/215168
prop_dollarCompose f g x = ((f . g) x) == ((\x' -> f $ g x') x)

main :: IO ()
main = do
  quickCheck ((prop_dollarCompose reverse sort) ::[Integer] -> Bool)
  quickCheck ((prop_dollarCompose reverse sort) ::[Int]     -> Bool)
  quickCheck ((prop_dollarCompose reverse sort) ::[Char]    -> Bool)
  quickCheck ((prop_dollarCompose reverse sort) ::[Double]  -> Bool)
  quickCheck ((prop_dollarCompose reverse sort) ::[Float]   -> Bool)
  quickCheck ((prop_dollarCompose reverse sort) ::[Bool]    -> Bool)

-- 9)
-- The first property is falsified with [0] [1] however all tests
-- pass with the second property test.
import Test.QuickCheck

prop_foldrConsPlusPlus :: Eq a => [a] -> [a] -> Bool
prop_foldrConsPlusPlus x y = (foldr (:) x y) == ((++) x y)

prop_foldrPlusPlusConcat :: (Eq a, Foldable t) => t [a] -> Bool
prop_foldrPlusPlusConcat x = (foldr (++) [] x) == concat x

main :: IO ()
main = do
  quickCheck (prop_foldrConsPlusPlus :: [Integer] -> [Integer] -> Bool)
  quickCheck (prop_foldrPlusPlusConcat :: [String] -> Bool)
  quickCheck (prop_foldrPlusPlusConcat :: [[Integer]] -> Bool)
  quickCheck (prop_foldrPlusPlusConcat :: [[Int]] -> Bool)
  quickCheck (prop_foldrPlusPlusConcat :: [[Double]] -> Bool)

-- 10)
-- Nope, doesn't work for n greater than the length of the
-- list. QuickCheck falsified it with 1 []
import Test.QuickCheck

prop_length :: Int -> [a] -> Bool
prop_length n xs = length (take n xs) == n

main :: IO ()
main = do
  quickCheck (prop_length :: Int -> [Integer] -> Bool)

-- 11)
-- All tests pass
import Test.QuickCheck

prop_readShow :: (Eq a, Read a, Show a) => a -> Bool
prop_readShow x = (read (show x)) == x

main :: IO ()
main = do
  quickCheck (prop_readShow :: Integer   -> Bool)
  quickCheck (prop_readShow :: Int       -> Bool)
  quickCheck (prop_readShow :: Double    -> Bool)
  quickCheck (prop_readShow :: Char      -> Bool)
  quickCheck (prop_readShow :: String    -> Bool)
  quickCheck (prop_readShow :: [Integer] -> Bool)
  quickCheck (prop_readShow :: [Int]     -> Bool)
  quickCheck (prop_readShow :: [Double]  -> Bool)

-- Failure

-- It fails because of loss of precision when carrying
-- out floating point operations.
import Test.QuickCheck

square :: Num a => a -> a
square x = x * x

squareIdentity :: (Eq a, Floating a) => a -> Bool
squareIdentity x = (square . sqrt) x == id x

main :: IO ()
main = do
  quickCheck (squareIdentity :: Double -> Bool)
  quickCheck (squareIdentity :: Float  -> Bool)

-- Idempotence

import Data.Char
import Data.List
import Test.QuickCheck

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

f :: String -> Bool
f x =
  (capitalizeWord x
  == twice capitalizeWord x)
  &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)

f' :: Ord a => [a] -> Bool
f' x =
  (sort x
  == twice sort x)
  &&
  (sort x
  == fourTimes sort x)

main :: IO ()
main = do
  quickCheck f
  quickCheck (f' :: [Integer] -> Bool)
  quickCheck (f' :: [Int]     -> Bool)
  quickCheck (f' :: [Double]  -> Bool)
  quickCheck (f' :: [Float]   -> Bool)
  quickCheck (f' :: [Char]    -> Bool)
  quickCheck (f' :: [Bool]    -> Bool)

-- Make a Gen random generator for the datatype

import Test.QuickCheck

data Fool =
  Fulse
  | Frue
  deriving (Eq, Show)

-- 1)
genFool :: Gen Fool
genFool = do
  oneof [return $ Fulse,
         return $ Frue]

-- 2)
genFool' :: Gen Fool
genFool' =
  frequency [(2, return Fulse),
             (1, return Frue)]

-- Hangman testing

-- First, had to add `deriving (Eq)` to Puzzle:
data Puzzle =
  Puzzle String [Maybe Char] [Char]
  deriving (Eq)

-- To test, worked in existing ch13 project directory. Added
-- hspec to hangman.cabal build-depends, then added this to Main.hs
-- (replacing existing main function):
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    context "a puzzle where the guessed char is in the puzzle" $ do
      it "fills in the character in the puzzle and adds char to guesses" $ do
        (fillInCharacter (freshPuzzle "foo") 'o') `shouldBe`
          Puzzle "foo" [Nothing, Just 'o', Just 'o'] "o"
    context "a puzzle where the guessed char is not in the puzzle" $ do
      it "doesn't fill in the puzzle but adds char to guesses" $ do
        (fillInCharacter (freshPuzzle "foo") 'z') `shouldBe`
          Puzzle "foo" [Nothing, Nothing, Nothing] "z"
  describe "handleGuess" $ do
    context "a puzzle with an existing guess" $ do
      let puzzle = (Puzzle "foo" [Nothing, Nothing, Nothing] "z")
      context "re-guessing the same char" $ do
        it "doesn't change the puzzle" $ do
          puzzle' <- handleGuess puzzle 'z'
          puzzle' `shouldBe` puzzle
      context "guessing a new char where the char is in the puzzle" $ do
        it "updates the puzzle and guesses" $ do
          puzzle' <- handleGuess puzzle 'f'
          puzzle' `shouldBe` (Puzzle "foo" [Just 'f', Nothing, Nothing] "fz")
      context "guessing a new char where the char is not in the puzzle" $ do
        it "updates the guesses but not the puzzle" $ do
          puzzle' <- handleGuess puzzle 'x'
          puzzle' `shouldBe` (Puzzle "foo" [Nothing, Nothing, Nothing] "xz")
    context "a puzzle without existing guesses" $ do
      let puzzle = (Puzzle "foo" [Nothing, Nothing, Nothing] "")
      context "guessing a new char where the char is in the puzzle" $ do
        it "updates the puzzle and guesses" $ do
          puzzle' <- handleGuess puzzle 'f'
          puzzle' `shouldBe` (Puzzle "foo" [Just 'f', Nothing, Nothing] "f")
      context "guessing a new char where the char is not in the puzzle" $ do
        it "updates the guesses but not the puzzle" $ do
          puzzle' <- handleGuess puzzle 'x'
          puzzle' `shouldBe` (Puzzle "foo" [Nothing, Nothing, Nothing] "x")

-- Validating ciphers

-- See "ciphers" directory:
-- cd ciphers
-- stack ghci Main.hs
-- > main

-- 13.6

-- Intermission: Check your understanding

-- 1)
-- forever and when

-- 2)
-- import Data.Bits
-- import Database.Blacktip.Types

-- 3)
-- I would guess it imports type definitions

-- 4a)
-- MV = Control.Concurrent.MVar
-- FPC = Filesystem.Path.CurrentOS
-- CC = Control.Concurrent

-- 4b)
-- import qualified Filesystem as FS

-- 4c)
-- import Control.Monad (forever, when)

-- 13.14

-- Modifying code

-- 1)
-- see vigenere.hs and caesar.hs `encodeInput` and `decodeInput` functions

-- 2)

import Control.Monad
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

-- 3)

import Control.Monad
import Data.Char (toLower)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let line1' = filter (\x -> x `elem` ['a'..'z']) (map toLower line1)
  case (line1' == reverse line1') of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

-- 4)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter your name:"
  name <- getLine
  putStrLn "Enter your age:"
  age <- getLine
  let age' = (read age) :: Integer
  case (mkPerson name age') of
    (Right x) -> do
      putStrLn "Yay! Successfully got a person:"
      putStrLn (show x)
    (Left x) -> do
      putStrLn "Error creating person:"
      putStrLn (show x)

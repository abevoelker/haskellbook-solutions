module Cipher where
import Data.Char
import Data.List

caesarDigits = ['a'..'z']

caesarChar :: Int -> Char
caesarChar x = caesarDigits !! (x `mod` (length caesarDigits))

caesarChars :: Int -> String
caesarChars x
  | i < 0 = take i caesarDigits ++ drop i caesarDigits
  | otherwise = drop i caesarDigits ++ take i caesarDigits
  where
    i = x `mod` (length caesarDigits)

caesarCharCipher :: Char -> Int -> Char
caesarCharCipher x y = case elemIndex x caesarDigits of
  Just i -> (caesarChars y) !! i
  Nothing -> x

caesarCharDecipher :: Char -> Int -> Char
caesarCharDecipher x y = case elemIndex x (caesarChars y) of
  Just i -> caesarDigits !! i
  Nothing -> x

caesar :: String -> Int -> String
caesar [] _ = []
caesar (x:xs) i = (caesarCharCipher x i) : (caesar xs i)

unCaesar :: String -> Int -> String
unCaesar [] _ = []
unCaesar (x:xs) i = (caesarCharDecipher x i) : (unCaesar xs i)

encodeInput :: IO ()
encodeInput = do
  putStrLn "Input plaintext to encode:"
  plaintext <- getLine
  putStrLn "Input shift"
  shift <- getLine
  let shift' = (read shift) :: Int
  putStrLn $ caesar plaintext shift'

decodeInput :: IO ()
decodeInput = do
  putStrLn "Input ciphertext to decode:"
  ciphertext <- getLine
  putStrLn "Input shift"
  shift <- getLine
  let shift' = (read shift) :: Int
  putStrLn $ unCaesar ciphertext shift'

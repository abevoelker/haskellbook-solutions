module Main where

import qualified Caesar as C
import qualified Vigenere as V
import Test.QuickCheck

caesarIdentity :: String -> Int -> Bool
caesarIdentity plaintext i = (C.unCaesar (C.caesar plaintext i) i) == plaintext

vigIdentity :: String -> String -> Bool
vigIdentity plaintext codeword = (V.decode (V.encode plaintext codeword) codeword) == plaintext

main :: IO ()
main = do
  quickCheck caesarIdentity
  quickCheck vigIdentity

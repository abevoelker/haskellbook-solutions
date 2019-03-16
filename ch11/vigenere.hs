module Vigenere where
import Data.Char

infiniteUpper :: String
infiniteUpper = concat $ (repeat ['A'..'Z'])

infiniteLower :: String
infiniteLower = concat $ (repeat ['a'..'z'])

-- charIdx 'A' = 0
-- charIdx 'B' = 1
charIdx :: Char -> Int
charIdx x
  | elem x ['A'..'Z'] = (ord x) - (ord 'A')
  | elem x ['a'..'z'] = (ord x) - (ord 'a')
  | otherwise = 0

-- charShift 'Y' 2 = 'A'
-- charShift 'y' 2 = 'a'
charShift :: Char -> Int -> Char
charShift x y
  | elem x ['A'..'Z'] = infiniteUpper !! ((charIdx x) + y + 26)
  | elem x ['a'..'z'] = infiniteLower !! ((charIdx x) + y + 26)
  | otherwise = x

encodeChar :: Char -> [Int] -> (Char, [Int])
encodeChar x encodeList
  | elem x (['A'..'Z'] ++ ['a'..'z']) = ((charShift x (head encodeList)), tail encodeList)
  | otherwise = (x, encodeList)

encodeString :: String -> [Int] -> String
encodeString "" _ = []
encodeString x  y = (fst charRes) : (encodeString (tail x) (snd charRes)) where
  charRes = encodeChar (head x) y

-- encode "MEET AT DAWN" "ALLY" = "MPPR AE OYWY"
encode :: String -> String -> String
encode plaintext codeword = encodeString plaintext encodeOffsets
  where codeword'     = concat $ (repeat codeword)
        encodeOffsets = (map charIdx codeword')

-- decode "MPPR AE OYWY" "ALLY" = "MEET AT DAWN"
decode :: String -> String -> String
decode ciphertext codeword = encodeString ciphertext decodeOffsets
  where codeword'     = concat $ (repeat codeword)
        negativeNum   = (\x -> x * (-1))
        decodeOffsets = (map (negativeNum . charIdx) codeword')

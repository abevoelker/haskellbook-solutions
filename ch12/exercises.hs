-- 12.5

-- Determine the kinds

-- 1)
-- a :: *

-- 2)
-- a :: *
-- f :: * -> *

-- String processing

-- 1)

notThe :: String -> Maybe String
notThe x
  | x == "the" = Nothing
  | otherwise  = Just x

replaceThe :: String -> String
replaceThe x = unwords $ map (replaceThe' . notThe) $ words x

replaceThe' :: Maybe String -> String
replaceThe' Nothing  = "a"
replaceThe' (Just x) = x

-- 2)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel x = go (words x) False where
  go :: [String] -> Bool -> Integer
  go []     _              = 0
  go (x:xs) lastWordWasThe = wordValue + (go xs (x == "the")) where
    wordValue = if (lastWordWasThe && isVowel (head x)) then 1 else 0

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiouAEIOU"

-- 3)

countVowels :: String -> Integer
countVowels = length (filter . isVowel)

-- Validate the word

newtype Word' =
  Word' String
  deriving (Eq, Show)

consonants = [x | x <- ['a'..'z'] ++ ['A'..'Z'], (not . isVowel) x]
isConsonant :: Char -> Bool
isConsonant x = x `elem` consonants

countConsonantsVowels :: String -> (Integer, Integer)
countConsonantsVowels x = go x (0,0) where
  go []     y = y
  go (x:xs) y@(cs,vs)
    | isConsonant x = go xs (cs + 1, vs)
    | isVowel     x = go xs (cs,     vs + 1)
    | otherwise     = go xs y

mkWord :: String -> Maybe Word'
mkWord x = if (vowels > consonants) then Nothing else (Just $ Word' x) where
  consonants = fst (countConsonantsVowels x)
  vowels = snd (countConsonantsVowels x)

-- It’s only Natural

data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + (natToInteger x)

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0     = Nothing
  | otherwise = Just (go x)
  where
    go x'
      | x' == 0   = Zero
      | otherwise = Succ (go (x' - 1))

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

-- Small library for Maybe

-- 1)

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

-- 2)

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just y) = (f y)

-- 3)

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just y) = y

-- 4)

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- 5)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
  Nothing   -> catMaybes(xs)
  (Just x') -> x' : catMaybes(xs)

-- 6)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe x = if (length $ catMaybes x) == (length x) then Just(catMaybes x) else Nothing

{-
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe x = if (fst result) then Nothing else (Just snd result) where
  go' True  _  = (True, undefined)
  go' x []     = (x,[])
  go' x (y:ys) = go' (x, )
  result = go' (False, [])
-}

-- Small library for Either

-- 1)

-- initial solution:
lefts' :: [Either a b] -> [a]
lefts' []     = []
lefts' (x:xs) = case x of
  (Left  x') -> x' : lefts'(xs)
  (Right x') -> lefts'(xs)

-- rewritten to use foldr:
leftToList :: Either a b -> [a]
leftToList (Left a) = [a]
leftToList _        = []

lefts'' :: [Either a b] -> [a]
lefts'' = foldr ((++) . leftToList) []

-- 2)
rightToList :: Either a b -> [b]
rightToList (Right b) = [b]
rightToList _        = []

rights' :: [Either a b] -> [b]
rights' = foldr ((++) . rightToList) []

-- 3)
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x = ((lefts' x), (rights' x))

-- 4)
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' _ (Left _)  = Nothing

-- 5)
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right y) = g y

-- 6)
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left x) = Nothing
eitherMaybe'' f (Right y) = Just(f y)

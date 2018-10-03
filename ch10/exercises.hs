-- 10.5

-- Undertanding Folds

-- 1) b and c

-- 2)

--(flip (*) (flip (*) (flip (*) 1 1) 2) 3)
--(flip (*) (flip (*) 1 2) 3)
--(flip (*) 2 3)
--6

-- 3) c

-- 4) a

-- 5) a)
foldr (++) "" ["woot", "WOOT", "woot"]

-- 5) b)
-- note: '\NUL' == (chr 0)
foldr max '\NUL' "fear is the little death"

-- 5) c)
foldr (&&) True [False, True]

-- 5) d)
-- No, the zero/identity being True will always short-circuit (||) to
-- return True regardless of other arguments. Replace with False to fix:
foldr (||) False [False, True]

-- 5) e)
foldr ((++) . show) "" [1..5]
-- or
foldl (flip $ (++) . show) "" [1..5]

-- 5) f)
foldl const 'a' [1..5]
-- or
foldr (flip const) 'a' [1..5]
-- or
foldr const 'a' ['1'..'5']

-- 5) g)
foldl const 0 "tacos"
-- or
foldr (flip const) 0 "tacos"
-- or
foldr const '0' "tacos"

-- 5) h)
foldl const 0 "burritos"
-- or
foldr (flip const) 0 "burritos"
-- or
foldl (flip const) '0' "burritos"

-- 5) i)
foldr (flip const) 'z' [1..5]
-- or
foldl const 'z' [1..5]


-- 10.6

-- Exercises: Database Processing

-- 1)
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\ a b -> case a of {DbDate x -> x : b; _ -> b}) []

-- 2)
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\ a b -> case a of {DbNumber x -> x : b; _ -> b}) []

-- 3)
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr (\ a b -> case a of {DbDate x -> if x > b then x else b; _ -> b}) (UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 0))

-- 4)
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (\ a b -> case a of {DbNumber x -> x + b; _ -> b}) 0

-- 5)
avgDb :: [DatabaseItem] -> Double
avgDb = (\x -> (fst x) / (snd x)) . (foldr (\ a b -> case a of {DbNumber x -> ((fromIntegral x) + (fst b), (snd b) + 1); _ -> b}) (0 :: Double, 0))


-- 10.9

-- Scans Exercises

-- 1)
fibs' = take 20 $ 1 : scanl (+) 1 fibs'

-- 2)
fibs'' = takeWhile (<100) $ 1 : scanl (+) 1 fibs''

-- 3)
factorial' = scanl (*) 1 [1..]


-- 10.10

-- Warm-up and review

-- 1) a)
[(x,y,z) | x <- stops, y <- vowels, z <- stops]

-- 1) b)
[(x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']

-- 1) c)
nouns = ["den", "expansionism", "extent", "handlebar", "opportunist", "parallelogram", "swamp", "tanker"]
verbs = ["stay", "paint", "look", "soak", "nod", "retire", "confuse", "kiss"]
[(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

-- 2)
-- It calculates the average word length of an input string. Its type is:
-- seekritFunc :: String -> Int

-- 3)
seekritFunc :: Fractional a => String -> a
seekritFunc x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))

-- Rewriting functions using folds

-- 1)
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\ a b -> (f a) || b) False

-- 3)
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\ a b -> (a == x) || b) False
-- I already wrote a version that uses `any` for chapter 9

-- 4)
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5)
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\ a b -> (f a) : b) []

-- 6)
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\ a b -> if (f a) then a : b else b) []

-- 7)
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\ a b -> (f a) ++ b) []

-- 9)
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldl (\a b -> if (f a b) == GT then a else b) (head xs) xs

-- 11)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldl (\a b -> if (f a b) == LT then a else b) (head xs) xs

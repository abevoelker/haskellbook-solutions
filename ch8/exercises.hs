-- 8.2

--   applyTimes 5 (+1) 5
-- = (+1) (applyTimes 4 (+1) 5)
-- = (+1) ((+1) (applyTimes 3 (+1) 5))
-- = (+1) ((+1) ((+1) (applyTimes 2 (+1) 5)))
-- = (+1) ((+1) ((+1) ((+1) (applyTimes 1 (+1) 5))))
-- = (+1) ((+1) ((+1) ((+1) ((+1) (5)))))
-- = 10


-- 8.6

-- Review of types

-- 1.
-- d)

-- 2.
-- b)

-- 3.
-- d)

-- 4.
-- b)

-- Reviewing currying

flippy :: String -> String -> String
appedCatty :: String -> String
frappe :: String -> String

-- 1.
-- "woops mrow woohoo!"

-- 2.
-- "1 mrow haha"

-- 3.
-- "woops mrow 2 mrow haha"

-- 4.
-- "woops mrow blue mrow haha"

-- 5.
-- "pink mrow haha mrow green mrow woops mrow blue"

-- 6.
-- "are mrow Pugs mrow awesome"

-- Recursion

-- 1.
--   dividedBy 15 2
-- = go 15 2 0
-- =   go 13 2 1
-- =     go 11 2 2
-- =       go 9 2 3
-- =         go 7 2 4
-- =           go 5 2 5
-- =             go 3 2 6
-- =               go 1 2 7
-- =                 (7, 1)

-- 2.
sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo max = go max 0 0 where
  go m carry i
    | i == (m + 1) = carry
    | otherwise = go m (carry + i) (i + 1)

-- 3.
flipIfOneNeg :: (Num a, Ord a) => (a, a) -> a -> a
flipIfOneNeg (x, y) z = if ((length . filter (\x -> x < 0) $ [x, y]) == 1) then (-z) else z

mult :: (Eq a, Ord a, Num a) => a -> a -> a
mult x y = go x' y' 0 where
  x' = abs(x)
  y' = abs(y)
  go n i carry
    | i == 0 = flipIfOneNeg (x, y) carry
    | otherwise = go n (i - 1) (carry + n)

-- Fixing dividedBy

data DividedResult =
  Result (Integer, Integer)
  | DividedByZero
  deriving Show

dividedBy' :: Integer -> Integer -> DividedResult
dividedBy' num denom
  | denom == 0 = DividedByZero
  | otherwise = go num' denom' 0 where
    num' = abs(num)
    denom' = abs(denom)
    go n d count
      | n < d = Result (flipIfOneNeg (num, denom) count, n)
      | otherwise = go (n - d) d (count + 1)

-- McCarthy 91 function

mc91 :: (Num a, Ord a) => a -> a
mc91 x = if (x > 100) then (x - 10) else mc91(mc91(x + 11))

-- Numbers into words
-- see wordnumber.hs

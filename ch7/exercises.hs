-- 7.4

-- Exercises: Grab Bag

-- 1.
-- they are all equivalent thanks to Haskell's automatic currying!

-- 2.
-- d)

-- 3.a)
addOneIfOdd' n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- 3.b)
addFive' = \x -> \y -> (if x > y then y else x) + 5

-- 3.c)
mflip' f x y = f y x


-- 7.4

-- Exercises: Variety Pack

-- 1.a)
k :: (a, b) -> a

-- 1.b)
k2 :: [Char]
-- no, k1 and k3 can be reduced to Num a => a

-- 1.c)
-- k1 and k3

-- 2.
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))


-- 7.5

-- Exercises: Case Practice

-- 1.
functionC' x y =
  case x > y of
    True -> x
    False -> y

-- 2.
ifEvenAdd2' n =
  case even n of
    True -> (n + 2)
    False -> n

-- 3.
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0


-- 7.6

-- Exercises: Artful Dodgy

-- 1.
dodgy :: Num a => a -> a -> a
oneIsOne :: Num a => a -> a
oneIsTwo :: Num a => a -> a
-- it evaluates to 1

-- 2.
-- 11

-- 3.
-- 22

-- 4.
-- 21

-- 5.
-- 12

-- 6.
-- 11

-- 7.
-- 21

-- 8.
-- 21

-- 9.
-- 22

-- 10.
-- 31

-- 11.
-- 23


-- 7.7

-- Exercises: Guard Duty

-- 1.
-- the otherwise clause matches anything, so if you put it first, it always
-- matches and in this case avgGrade always returns 'F'

-- 2.
-- it still typechecks, but returns incorrect result since the 'C' clause
-- matches earlier than it should, and now 'A' and 'B' will never match.

-- 3.
-- b)

-- 4.
-- Eq a => [a]

-- 5.
pal :: Eq a => [a] -> Bool

-- 6.
-- c)

-- 7.
-- (Num a, Ord a) => a

-- 8.
numbers :: (Num a, Ord a, Num b) => a -> b


-- 7.11

-- Multiple choice

-- 1.
-- d)

-- 2.
-- b)

-- 3.
-- d)

-- 4.
-- b)

-- 5.
-- a)

-- Let’s write code

-- 1.a)
tensDigit :: Integral a => a -> a
tensDigit x = d
  where (xLast, _) = x `divMod` 10
        (_, d)     = xLast `divMod` 10

-- 1.b)
-- Yes

-- 1.c)
hunsD :: Integral a => a -> a
hunsD x = d
  where d = (x `div` 100) `mod` 10
-- or, pointfree:
hunsD = (`mod` 10) . (`div` 100)

-- 2.
foldBool :: a -> a -> Bool -> a
-- case:
foldBool x y z =
  case z of
    True -> x
    False -> y
-- guard:
foldBool x y z
  | z == True = x
  | otherwise = y

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f(a), c)

-- 4.
-- (okay)

-- 5.
roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

-- 6.
-- I think you just need to use :: when calling the function (to be explicit):
-- roundTrip 5 :: Int
-- roundTrip 5 :: Integer
-- etc.

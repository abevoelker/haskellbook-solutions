-- 4.2

-- 1.
-- Mood

-- 2.
-- either Blah or Woot

-- 3.
-- you can't use a data constructor (Woot) in the type signature. should be
-- changeMood :: Mood -> Mood

-- 4.
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

-- 5.
-- see change_mood.hs


-- 4.4

-- 1.
not True && True

-- 2.
not (x == 6)

-- 3.
-- no mistake

-- 4.
["Merry"] > ["Happy"]

-- 5.
-- not sure what the intent of this code was? best guess:
['1', '2', '3'] ++ "look at me!"


-- 4.7

-- 1.
length :: [a] -> Integer
-- it takes one argument of type List, returning an Integer

-- 2.a)
-- 5

-- 2.b)
-- 3

-- 2.c)
-- 2

-- 2.d)
-- 5

-- 3.
-- `length` returns an Int, which isn't compatible w/ `(/)`'s type signature

-- 4.
6 `div` length [1, 2, 3]

-- 5.
-- Bool, True

-- 6.
-- Bool, False

-- 7.
-- length allAwesome == 2
-- ^ will work, because Int is an instance of Num (which is what 2 is). result
-- will be True
--
-- length [1, 'a', 3, 'b']
-- ^ will not compile because not all elements of the list have the same type
--
-- length allAwesome + length awesome
-- ^ will work, answer will be 5
--
-- (8 == 8) && ('b' < 'a')
-- ^ will work since both expressions passed to `(&&)` are Bools. result: False
--
-- (8 == 8) && 9
-- ^ will not compile since `(&&)` expects Bool arguments

-- 8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9.
myAbs :: Integer -> Integer
myAbs x = if x < 0 then x * (-1) else x

-- 10.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- Reading syntax

-- 1.
x = (+)
f xs = w `x` 1
  where w = length xs

-- 2.
(\x -> x)

-- 3.
(\(x:xs) -> x)

-- 4.
f (a, b) = a

-- Match the function names to their types

-- 1.
-- c)

-- 2.
-- b)

-- 3.
-- a)

-- 4.
-- d)


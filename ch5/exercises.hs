-- 5.4

-- 1.a)
-- c)

-- 1.b)
-- d)

-- 1.c)
-- b)

-- 1.d)
-- a)

-- 1.e)
-- e)


-- 5.5

-- 1
-- a)

-- 2.
-- d)

-- 3.
-- d)

-- 4.
-- c)

-- 5.
-- a)

-- 6.
-- e)

-- 7.
-- d)

-- 8.
-- a)

-- 9.
-- c)


-- 5.6

-- 1.
-- (ok)

-- 2.
ex2a :: a -> a -> a
ex2a x y = x

ex2b :: a -> a -> a
ex2b x y = y

-- 3.
ex3 :: a -> b -> b
ex3 x y = y
-- only one implementation because we have to assume a != b


-- 5.7

-- 1.
myConcat :: [Char] -> [Char]

-- 2.
myMult :: Fractional a => a -> a

-- 3.
myTake :: Int -> [Char]

-- 4.
myCom :: Int -> Bool

-- 5.
myAlph :: Char -> Bool


-- 5.9

-- Multiple choice

-- 1.
-- c)

-- 2.
-- a)

-- 3.
-- b), but also a) if you consider currying and it returning a function (Int -> a)

-- 4.
-- c)

-- Determine the type

-- 1.a)
54 :: Num a => a

-- 1.b)
(0,"doge") :: Num a => (a, [Char])

-- 1.c)
(0 :: Integer, "doge") :: (Integer, [Char])

-- 1.d)
False :: Bool

-- 1.e)
5 :: Int

-- 1.f)
False :: Bool

-- 2.
100 :: Num a => a

-- 3.
Num a => a -> a

-- 4.
0.4 :: Fractional a => a

-- 5.
"Julie <3 Haskell" :: [Char]

-- Does it compile?

-- 1.
bigNum = (^) 5 $ 10
wahoo = bigNum ^ 10

-- 2.
y = print "woohoo!"
z = x "hello world"

-- 3.
c = a b 10
d = a c 200

-- 4.
c = 5 -- does adding an add'tl expression count as fixing?

-- Type variable or specific type constructor?

-- 1.
-- (answer provided)

-- 2.
-- f :: zed -> Zed -> Blah
--     [1]    [2]    [3]
-- 1 = fully polymorphic
-- 2 = concrete
-- 3 = concrete

-- 3.
-- f :: Enum b => a -> b -> C
--               [1]  [2]  [3]
-- 1 = fully polymorphic
-- 2 = constrained polymorphic
-- 3 = concrete

-- 4.
-- f :: f -> g -> C
--     [1]  [2]  [3]
-- 1 = fully polymorphic
-- 2 = fully polymorphic
-- 3 = concrete

-- Write a type signature

-- 1.
functionH :: [a] -> a

-- 2.
functionC :: Ord a => a -> a -> Bool

-- 3.
functionS :: (a, b) -> b

-- Given a type, write the function

-- 1.
i x = x

-- 2.
c x y = x

-- 3.
-- yes:
c'' x y = x

-- 4.
c' x y = y

-- 5.
r xs = reverse xs

-- 6.
co f g = \x -> f (g x)
-- or
co f g = f . g

-- 7.
a :: (a -> c) -> a -> a
a f x = x

-- 8.
a' :: (a -> b) -> a -> b
a' f x = f x

-- Fix it

-- 1.
-- see sing.hs

-- 2.
-- see sing2.hs

-- Type-Kwon-Do

-- 1.
h x = g(f(x))
-- or
h x = g $ f $ x

-- 2.
e x = w(q(x))
-- or
e x = w $ q $ x

-- 3.
xform x = (xz(fst x), yz(snd x))

-- 4.
munge f g x = fst(g(f(x)))
-- or
munge f g x = fst $ g $ f $ x

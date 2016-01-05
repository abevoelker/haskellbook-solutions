-- 6.5

-- Integral
--
-- I'm guessing `quotRem` combines `quot` and `rem`, combining the results into
-- a single pair. Same with `divMod` but combining `div` and `mod`. REPL
-- experimentation seems to confirm the hypothesis.


-- 6.7

-- 1.
-- will work; returns 5

-- 2.
-- will work; returns LT

-- 3.
-- will not work; `compare` expects same types as args, but given [Char] and Bool

-- 4.
-- will work; returns False


-- 6.12

-- 1.
instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn x') = x == x'

-- 2.
instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = (x == x') && (y == y')

-- 3.
instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt x') = x == x'
  (==) (TisAString x) (TisAString x') = x == x'
  (==) _ _ = False

-- 4.
instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = (x == x') && (y == y')

-- 5.
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = (x == x') && (y == y')

-- 6.
instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _ = False

-- 7.
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye x) (Goodbye x') = x == x'
  (==) _ _ = False


-- 6.14

-- Multiple choice

-- 1.
-- c)

-- 2.
-- b)

-- 3.
-- a)

-- 4.
-- c)

-- 5.
-- a)

-- Does it typecheck?

-- 1.
-- no, use of putStrLn means Person needs an instance of Show. fix with:
data Person = Person Bool deriving Show

-- 2.
-- no, use of (==) means Mood needs an Eq instance. fix with:
data Mood = Blah
          | Woot deriving (Show, Eq)

-- 3.a)
-- only Mood types

-- 3.b)
-- it won't compile, since only Mood input types are acceptable

-- 3.c)
-- it will err because there is no Ord instance for Mood

-- 4.
-- I wasn't sure about s1, but apparently yes, data constructors can be
-- partially applied. So it typechecks.

-- Given a datatype declaration, what can we do?

-- 1.
-- Won't typecheck. Rocks and Yeah aren't type aliases, they're custom types
-- of their own so you need to use their data constructors. Fix with:
phew = Papu (Rocks "chases") (Yeah True)

-- 2.
-- Will typecheck

-- 3.
-- Will typecheck

-- 4.
-- Won't typecheck. Papu doesn't have an Ord instance.

-- Match the types

-- 1.
-- No

-- 2.
-- No

-- 3.
-- Yes

-- 4.
-- Yes

-- 5.
-- Yes

-- 6.
-- Yes

-- 7.
-- No

-- 8.
-- No

-- 9.
-- Yes

-- 10.
-- Yes

-- 11.
-- No

-- Type-Kwon-Do

-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f(x) == y

-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = f(y) + fromIntegral(x)

-- 6.5

-- Exercises: Eq Instances

-- 1.

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn x') = x == x'

-- 2.

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = (x == x') && (y == y')

-- 3.

data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x)   (TisAnInt x')   = x == x'
  (==) (TisAString x) (TisAString x') = x == x'
  (==) _ _ = False

-- 4.

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = (x == x') && (y == y')

-- 5.

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = (x == x') && (y == y')

-- 6.

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'

-- 7.

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye x) (Goodbye x') = x == x'
  (==) _ _ = False


-- 6.6

-- Exercises: Tuple Experiment

-- I'm guessing `quotRem` combines `quot` and `rem`, combining the results into
-- a single pair. Same with `divMod` but combining `div` and `mod`. REPL
-- experimentation seems to confirm the hypothesis.

-- Put on your thinking cap

-- Because Fractional is a subclass of Num, so Num is implicitly required.


-- 6.8

-- Exercises: Will They Work?

-- 1.
-- will work; returns 5

-- 2.
-- will work; returns LT

-- 3.
-- will not work; `compare` expects same types as args, but given [Char] and Bool

-- 4.
-- will work; returns False


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
-- No, because 1 :: Num a => a, so it is not parametrically polymorphic

-- 2.
-- No, the maximally polymorphic type of 1.0 is Fractional a => a.
-- Fractional is a subclass of Num, so you can't go up the ladder and make
-- it more general.

-- 3.
-- Yes, Fractional a => a is maximally polymorphic for 1.0 so that's fine.

-- 4.
-- Yes, this should work because RealFrac is a subclass of Fractional, so it's
-- okay to constrain the type further.

-- 5.
-- Yes, you can constrain the identity function.

-- 6.
-- Yes, you can constrain the identity function.

-- 7.
-- No, myX is a concrete type (Int), so at best you can do sigmund :: a -> Int

-- 8.
-- No, same answer as #7. The Num a constraint on the argument is okay but the
-- result must be Int (i.e. sigmund' :: Num a => a -> Int would be okay)

-- 9.
-- Yes, Int has an Ord instance so that's fine to constrain it

-- 10.
-- Yes, head :: [a] -> a and sort :: Ord a => [a] -> [a] so
-- young :: Ord a => [a] -> [a] is the maximally polymorphic type

-- 11.
-- No, mySort is constrained to concrete types ([Char] -> [Char])


-- Type-Kwon-Do

-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f(x) == y

-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = fromInteger(x) + f(y)

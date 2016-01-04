-- 3.3

-- 1.
-- yes

-- 2.
-- no

-- 3.
-- no (d out of scope in 2nd line)

-- 4.
-- yes


-- 3.4

-- 1.
-- no, incorrect use of infix notation. fix w/ prefix notation:
(++) [1, 2, 3] [4, 5, 6]

-- 2.
-- no, single quotes are used for chars, not strings. fixed:
"<3" ++ " Haskell"

-- 3.
-- yes, String is a list of Chars, so a list of Strings type checks `concat`


-- 3.5
-- (see print4fixed.hs)


-- 3.7

-- Reading syntax

-- 1.a)
-- correct

-- 1.b)
-- incorrect. fixed:
[1, 2, 3] ++ [4, 5, 6]
-- or
(++) [1, 2, 3] [4, 5, 6]

-- 1.c)
-- correct

-- 1.d)
-- incorrect. fixed:
["hello" ++ " world"]

-- 1.e)
-- incorrect. fixed:
"hello" !! 4

-- 1.f)
-- correct

-- 1.g)
-- incorrect. fixed:
take 4 "lovely"

-- 1.h)
-- correct

-- 2.a)
-- d

-- 2.b)
-- c

-- 2.c)
-- e

-- 2.d)
-- a

-- 2.e)
-- b

-- Building functions

-- 1.a)
"Curry is awesome" ++ "!"

-- 1.b)
["Curry is awesome!" !! 4]
-- or
take 1 $ drop 4 "Curry is awesome!"

-- 1.c)
drop 9 "Curry is awesome!"

-- 2.
-- see building_functions.hs
-- ex1a "Curry is awesome"
-- ex1b "Curry is awesome!"
-- ex1c "Curry is awesome!"

-- 3.
thirdLetter :: String -> Char
thirdLetter x = x !! 2

-- 4.
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

-- 5.
rvrs :: String -> String
rvrs x = (drop 9 x) ++ " " ++ (take 2 $ drop 6 x) ++ " " ++ (take 5 x)

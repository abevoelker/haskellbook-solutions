-- 2.4

-- 1.
let half x = x / 2
let square x = x * x

-- 2.
let ex242 x = 3.14 * (x * x)


-- 2.5

-- 1.
-- yes

-- 2.
-- no

-- 3.
-- yes


-- 2.6

-- 1.
let area x = 3.14 * (x * x)

-- 2.
let double x = x * 2

-- 3.
x = 7
y = 10
f = x + y


-- 2.12

ex1 = x * 3 + y
  where x = 3
        y = 1000

ex2 = x * 5
  where x = 10 * 5 + y
        y = 10

ex3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10


-- 2.13

-- Parenthesization

-- 2 + 2 * 3 - 1
2 + (2 * 3) - 1

-- (^) 10 $ 1 + 1
(^) 10 (1 + 1)

-- 2 ^ 2 * 4 ^ 5 + 1
(2 ^ 2) * (4 ^ 5) + 1

-- Equivalent expressions

-- 1.
-- same

-- 2.
-- same

-- 3.
-- different

-- 4.
-- different (integer vs. fractional division)

-- 5.
-- different

-- More fun with functions

-- 0.
let z = 7
let y = z + 8
let x = y ^ 2
let waxOn = x * 5
-- waxOn = 1125

-- 1.

-- 10 + waxOn
-- 1135

-- (+10) waxOn
-- 1135

-- (-) 15 waxOn
-- -1110

-- (-) waxOn 15
-- 1110

-- 2.
-- (okay)

-- 3.
-- the x in the triple function is local to it, so it will evaluate (1125 * 3)
-- which is 3375

-- 4.
-- see wax_on.hs

-- 5.
-- yep, still works

-- 6.
-- (okay)

-- 7.
-- What is the result of waxOff 10 or waxOff (-50)?
-- 30 and -150

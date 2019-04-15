module UsingQuickCheck where

-- 1)

import Test.QuickCheck

half x = x / 2
halfIdentity = (*2) . half

prop_half :: Double -> Bool
prop_half x = x == halfIdentity x

main :: IO ()
main = quickCheck prop_half

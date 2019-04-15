module ShortExercise where

import Test.Hspec

-- resurrected from chapter 8 exercises
mult :: (Integral a) => a -> a -> a
mult x y = go x y 0 where
  go x' remainingCount accum
    | remainingCount == 0 = accum
    | otherwise = go x' (remainingCount - 1) (accum + x')

-- Intermission: Short Exercise
main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 * 1 is 1" $ do
      mult 1 1 `shouldBe` 1
    it "2 * 5 is 10" $ do
      mult 2 5 `shouldBe` 10

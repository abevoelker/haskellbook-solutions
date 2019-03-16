module Jammin where
import Data.List

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars =
  Jam { fruit :: Fruit
      , count :: Int }
      deriving (Eq, Show, Ord)

row1 = Jam Plum 8
row2 = Jam Blackberry 5
row3 = Jam Apple 3
row4 = Jam Peach 10
row5 = Jam Apple 9
row6 = Jam Peach 3
allJam = [row1, row2, row3, row4, row5, row6]

totalJars :: [JamJars] -> Int
totalJars = sum . (map count)

totalJars' :: [JamJars] -> Int
totalJars' = foldr (\a b -> count a + b) 0

mostRow :: [JamJars] -> JamJars
mostRow xs = maximumBy (\x y -> compare (count x) (count y)) xs

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

compareCount :: JamJars -> JamJars -> Ordering
compareCount (Jam _ c) (Jam _ c') = compare c' c

sameKind :: JamJars -> JamJars -> Bool
sameKind (Jam k _) (Jam k' _) = k == k'

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy sameKind . (sortBy compareKind) . (sortBy compareCount)

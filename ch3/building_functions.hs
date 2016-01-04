module BuildingFunctions where

ex1a :: String -> String
ex1a x = x ++ "!"

ex1b :: String -> String
ex1b x = [x !! 4]

ex1c :: String -> String
ex1c x = drop 9 x

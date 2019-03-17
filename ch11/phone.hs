{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord

convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"]

type Digit = Char
type Presses = Int

data PhoneButton =
  PhoneButton { idDigit     :: Digit
              , extraDigits :: [Digit] }
      deriving (Eq, Show)

--type VanillaPhoneButton = PhoneButton
--type CapitalizePhoneButton = PhoneButton

class BtnPress a where
  btnPress       :: a -> Char -> [(Digit, Presses)]
  getIdDigit     :: a -> Digit
  getExtraDigits :: a -> [Digit]

instance BtnPress PhoneButton where
  getIdDigit x = idDigit x
  getExtraDigits x = extraDigits x
  btnPress x y = go x y 1 ((extraDigits x) ++ [idDigit x]) where
    go x y i []     = []
    go x y i (z:zs) = if (z == y) then [(getIdDigit x, i)] else go x y (i + 1) zs

newtype VanillaPhoneButton = VanillaPhoneButton PhoneButton
  deriving (Eq,Show,BtnPress)
newtype CapitalizePhoneButton = CapitalizePhoneButton PhoneButton
  deriving (Eq,Show)

-- capitalize button requires one extra press to go past uppercase
-- modification mode and into the available digits
instance BtnPress CapitalizePhoneButton where
  getIdDigit (CapitalizePhoneButton x) = idDigit x
  getExtraDigits (CapitalizePhoneButton x) = extraDigits x
  btnPress (CapitalizePhoneButton x) y = go x y 2 ((extraDigits x) ++ [idDigit x]) where
    go x y i []     = []
    go x y i (z:zs) = if (z == y) then [(z, i)] else go x y (i + 1) zs

data DaPhone =
  DaPhone CapitalizePhoneButton [VanillaPhoneButton]
  deriving (Eq, Show)

defaultPhone = DaPhone
  (CapitalizePhoneButton PhoneButton { idDigit = '*', extraDigits = ['^'] })
  [ VanillaPhoneButton PhoneButton { idDigit = '1', extraDigits = [] },
    VanillaPhoneButton PhoneButton { idDigit = '2', extraDigits = ['a'..'c'] },
    VanillaPhoneButton PhoneButton { idDigit = '3', extraDigits = ['d'..'f'] },
    VanillaPhoneButton PhoneButton { idDigit = '4', extraDigits = ['g'..'i'] },
    VanillaPhoneButton PhoneButton { idDigit = '5', extraDigits = ['j'..'l'] },
    VanillaPhoneButton PhoneButton { idDigit = '6', extraDigits = ['m'..'o'] },
    VanillaPhoneButton PhoneButton { idDigit = '7', extraDigits = ['p'..'s'] },
    VanillaPhoneButton PhoneButton { idDigit = '8', extraDigits = ['t'..'v'] },
    VanillaPhoneButton PhoneButton { idDigit = '9', extraDigits = ['w'..'z'] },
    VanillaPhoneButton PhoneButton { idDigit = '0', extraDigits = [' ', '+', '_'] },
    VanillaPhoneButton PhoneButton { idDigit = '#', extraDigits = ['.', ','] } ]

--allDigits :: PhoneButton
--allDigits x = (extraDigits x) ++ [idDigit x]

--getBtnPress :: PhoneButton -> Char -> [(Digit, Presses)]
--getBtnPress (CapitalizePhoneButton x) = undefined

getCapitalizeBtn :: DaPhone -> CapitalizePhoneButton
getCapitalizeBtn (DaPhone x _) = x

--getIdDigit :: CapitalizePhoneButton -> Digit
--getIdDigit (CapitalizePhoneButton x) = idDigit x

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps x@(DaPhone capBtn vanBtns) y
  | isUpper y = (getIdDigit capBtn, 1) : (reverseTaps x (toLower y))
  | length (btnPress capBtn y) > 0 = btnPress capBtn y
  | otherwise = btnPress (fromJust (find (\x' -> length (btnPress x' y) > 0) vanBtns)) y

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead x y = concat $ map (reverseTaps x) y

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps x = sum (map snd x)

mostPopularLetter :: String -> Char
mostPopularLetter x = fst (maximumBy (comparing snd) letterCounts) where
  letterCounts = map (\x' -> (head x', length x')) $ group $ sort x

mostPopularLetterCosts = map (\x -> (x, (fingerTaps $ (reverseTaps defaultPhone x)))) (map mostPopularLetter convo)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord x = fst (maximumBy (comparing snd) wordCounts) where
  allWords = concat (map words x)
  wordCounts = map (\x' -> (head x', length x')) $ group $ sort allWords


-- keeping the very first attempt for posterity, written before thinking much
-- about how to design data structures to store the phone digits
{-
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps x y
  | y `elem` ['A'..'Z'] = ('*', 1) : reverseTaps x (toLower y)
  | isJust idxBtn1     = [('1' :: Digit, (fromJust idxBtn1) + 1 :: Presses)]
  | isJust idxBtn2     = [('2' :: Digit, (fromJust idxBtn2) + 1 :: Presses)]
  | isJust idxBtn3     = [('3' :: Digit, (fromJust idxBtn3) + 1 :: Presses)]
  | isJust idxBtn4     = [('4' :: Digit, (fromJust idxBtn4) + 1 :: Presses)]
  | isJust idxBtn5     = [('5' :: Digit, (fromJust idxBtn5) + 1 :: Presses)]
  | isJust idxBtn6     = [('6' :: Digit, (fromJust idxBtn6) + 1 :: Presses)]
  | isJust idxBtn7     = [('7' :: Digit, (fromJust idxBtn7) + 1 :: Presses)]
  | isJust idxBtn8     = [('8' :: Digit, (fromJust idxBtn8) + 1 :: Presses)]
  | isJust idxBtn9     = [('9' :: Digit, (fromJust idxBtn9) + 1 :: Presses)]
  | isJust idxBtn0     = [('0' :: Digit, (fromJust idxBtn0) + 1 :: Presses)]
  | isJust idxBtnStar  = [('*' :: Digit, (fromJust idxBtnStar) + 1 :: Presses)]
  | isJust idxBtnPound = [('#' :: Digit, (fromJust idxBtnPound) + 1 :: Presses)]
  | otherwise         = []
  where
    idxBtn1     = y `elemIndex` ['1']
    idxBtn2     = y `elemIndex` (['a'..'c'] ++ ['2'])
    idxBtn3     = y `elemIndex` (['d'..'f'] ++ ['3'])
    idxBtn4     = y `elemIndex` (['g'..'i'] ++ ['4'])
    idxBtn5     = y `elemIndex` (['j'..'l'] ++ ['5'])
    idxBtn6     = y `elemIndex` (['m'..'o'] ++ ['6'])
    idxBtn7     = y `elemIndex` (['p'..'s'] ++ ['7'])
    idxBtn8     = y `elemIndex` (['t'..'v'] ++ ['8'])
    idxBtn9     = y `elemIndex` (['w'..'z'] ++ ['9'])
    idxBtn0     = y `elemIndex` (['+', '_'] ++ ['0'])
    idxBtnStar  = y `elemIndex` ['^', '*']
    idxBtnPound = y `elemIndex` ['.', ',', '#']
-}

module Ch9 where

import Data.Char

eft :: Enum a => a -> a -> [a]
eft from to = go from to []
  where
    go f t l
      | fromEnum f >= fromEnum t = l
    go f t []                    = go f t [t]
    go f t (b:bs)
      | fromEnum f >= fromEnum b = b:bs
      | otherwise                = go f t ((pred b):b:bs)

-- eftBool :: Bool -> Bool -> [Bool]
-- eftBool = undefined

-- eftOrd :: Ordering -> Ordering -> [Ordering]
-- eftOrd = undefined

-- eftInt :: Int -> Int -> [Int]
-- eftInt = undefined

-- eftChar :: Char -> Char -> [Char]
-- eftChar = undefined

--

splitAtChar :: Char -> String -> [String]
splitAtChar _ "" = [""]
splitAtChar splitChar string = go [] string
  where
    go ws ""            = ws
    go ws (c:cs)
      | c == splitChar = go ws cs
    go ws cs            = go (ws ++ [takeWhile (/= splitChar) cs]) (dropWhile (/= splitChar) cs)

myWords :: String -> [String]
myWords = splitAtChar ' '

myLines :: String -> [String]
myLines = splitAtChar '\n'

-- mySqr = [x^2 | x <- [1..10]]
test = [x | x <- mySqr, rem x 2 == 0]

test2 = [(x, y) | x <- mySqr,
  y <- mySqr,
  x < 50, y > 50]

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
squareCube = [(x, y) | x <- mySqr,
  y <- myCube,
  x < 50,
  y < 50]

getCapChars :: String -> String
getCapChars = filter $ flip elem $ ['A'..'Z']

capFirstChar :: String -> String
capFirstChar "" = ""
capFirstChar (c:cs) = (toUpper c):cs

capitalize :: String -> String
capitalize "" = ""
capitalize str = [toUpper . head $ str]

---

-- direct recursion, not using (&&)
-- myAnd :: [Bool] -> Bool
-- myAnd [] = True
-- myAnd (x:xs) =
--   if x == False
--   then False
--   else myAnd xs

-- direct recursion, using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny toBool as = myOr $ map toBool $ as

myElemViaRecursion :: Eq a => a -> [a] -> Bool
myElemViaRecursion _ [] = False
myElemViaRecursion x (y:ys) = y == x || myElemViaRecursion x ys

myElemViaAny :: Eq a => a -> [a] -> Bool
myElemViaAny x = any (== x)

module WordNumber (digitToWord, digits, wordNumber) where

import Data.List (intercalate)

digitWords :: [String]
digitWords =
  [
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine"
  ]

digitToWord :: Int -> String
digitToWord n
  | n >= 0 && n <= 9 = digitWords !! n
  | otherwise        = ""

digits :: Int -> [Int]
digits int = go int []
  where
    go n ds
      | n >= 10   = go (n `div` 10) $ (n `rem` 10):ds
      | otherwise = n:ds

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

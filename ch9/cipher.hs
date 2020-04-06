module Cipher where

import Data.Char

smallAInt :: Int
smallAInt = fromEnum 'a'

smallZInt :: Int
smallZInt = fromEnum 'z'

alphLen :: Int
alphLen = 26

shift :: Int -> Char -> Char
shift n c
  | not $ isAlpha c = c
  | isUpper c       = toUpper $ shift n $ toLower c
  | moduloShift < 0 = toEnum $ smallZInt + moduloShift
  | otherwise       = toEnum $ smallAInt + moduloShift
  where
    posFromA = (fromEnum c) - smallAInt
    moduloShift = mod (posFromA + n) alphLen

cipher :: Int -> String -> String
cipher n str = (map $ shift n) str

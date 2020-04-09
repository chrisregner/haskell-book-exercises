module Cipher where

import Data.Char

smallAInt :: Int
smallAInt = fromEnum 'a'

smallZInt :: Int
smallZInt = fromEnum 'z'

alphLen :: Int
alphLen = 26

getCharIntBase26 :: Char -> Int
getCharIntBase26 char
  | isAlpha char = (+ (- smallAInt)) . fromEnum . toLower $ char
  | otherwise    = 0 -- do not shift non-alpha chars

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

vigenereCipher :: String -> String -> String
vigenereCipher keyword = fst . foldl f ("", 0)
  where
    f :: (String, Int) -> Char -> (String, Int)
    f ("",     0) char =
      ([vigenereShift 0 char]
      , addIfAlph char 0)
    f (str, pos) char =
      (str ++ [(vigenereShift (keywordPos pos) char)]
      , addIfAlph char pos
      )

    keywordPos :: Int -> Int
    keywordPos pos = mod (pos) (length keyword)

    addIfAlph :: Char -> Int -> Int
    addIfAlph c n
      | isAlpha c = n + 1
      | otherwise = n

    vigenereShift :: Int -> Char -> Char
    vigenereShift n = shift (getCharIntBase26 $ keyword !! n)

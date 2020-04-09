import Data.List
import Data.Maybe

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str   = Just str

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
--
replaceThe :: String -> String
replaceThe = intercalate " " . map (\w -> maybe "a" id . notThe $ w) . words

-- >>> countTheBeforeVowel "the cow"
-- 0
--
-- >>> countTheBeforeVowel "the evil cow"
-- 1
--
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countTheBeforeVowel' 0 . words
  where
    countTheBeforeVowel' n []                = n
    countTheBeforeVowel' n (_:[])            = n
    countTheBeforeVowel' n (w:w':ws)
      | w == "the" && elem (head w') "aeiou" = countTheBeforeVowel' (n + 1) ws
      | otherwise                            = countTheBeforeVowel' n ws

-- >>> countVowels "the cow"
-- 2
--
-- >>> countVowels "Mikolajczak"
-- 4
--
countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel

isVowel :: Char -> Bool
isVowel = flip elem $ "aeiou"

countConsonants :: String -> Integer
countConsonants = fromIntegral . length . filter isConsonant

-- >>> isConsonant 'a'
-- False
--
isConsonant :: Char -> Bool
isConsonant = not . flip elem "aeiou"

newtype Word' =
  Word' String
  deriving (Eq, Show)

-- >>> mkWord "abcdee"
-- Just (Word' "abcdee")
--
mkWord :: String -> Maybe Word'
mkWord w
  | countVowels w <= countConsonants w = Just $ Word' w
  | otherwise                          = Nothing

-- As natural as any
-- competitive bodybuilder
data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
--
-- >>> natToInteger (Succ Zero)
-- 1
--
-- >>> natToInteger (Succ (Succ Zero))
-- 2
--
natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ nat) = 1 + natToInteger nat

-- >>> integerToNat 0
-- Just Zero
--
-- >>> integerToNat 1
-- Just (Succ Zero)
--
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
--
-- >>> integerToNat (-1)
-- Nothing
--
integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat n
  | n < 0     = Nothing
  | otherwise = Just $ composeSelf Succ (fromIntegral n) Zero

composeSelf :: (a -> a) -> Int -> a -> a
composeSelf f n xs = iterate f xs !! n

--

-- >>> myCatMaybes [Just 1, Nothing, Just 2]
-- [1,2]
--
-- >>> let xs = take 3 $ repeat Nothing
-- >>> myCatMaybes xs
-- []
--
myCatMaybes :: [Maybe a] -> [a]
myCatMaybes = map fromJust . filter isJust

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1,2,3]
--
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
--
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr flipMaybe' (Just [])
  where
    flipMaybe' Nothing _ = Nothing
    flipMaybe' _ Nothing = Nothing
    flipMaybe' (Just a) (Just as) = Just (a:as)

-- >>> lefts' [Left 'a', Left 'b', Right 'c']
-- "ab"
--
lefts' :: [Either a b] -> [a]
lefts' = foldr collectLeft []
  where
    collectLeft :: Either a b -> [a] -> [a]
    collectLeft (Left a) as = a:as
    collectLeft _ as        = as

-- >>> partition' [Left 'a', Left 'b', Right 'c']
-- ("ab","c")
--
partition' :: [Either a b] -> ([a], [b])
partition' = foldr collectPartitions ([], [])
  where
    collectPartitions :: Either a b -> ([a], [b]) -> ([a], [b])
    collectPartitions (Left l)  (ls, rs) = (l:ls, rs)
    collectPartitions (Right r) (ls, rs) = (ls, r:rs)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = [f x] ++ myIterate f (f x)

-- >>> take 10 $ myIterate' (+1) 0
-- [0,1,2,3,4,5,6,7,8,9]
--
myIterate' :: (a -> a) -> a -> [a]
myIterate' f x = myUnfoldr (\x' -> Just (x', f x')) x

-- >>> take 10 $ myUnfoldr (\b -> Just (b, b+1)) 0
-- [0,1,2,3,4,5,6,7,8,9]
--
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr bToMab b = case bToMab b of
  Nothing      -> []
  Just (a, b') -> a:(myUnfoldr bToMab b')

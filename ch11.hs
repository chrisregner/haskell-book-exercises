{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
-- import Ch9
import Data.List

type A = String
type B = String

appendA :: A -> A
appendA = (++ "_APPENDED")

b :: B
b = "B"

test = appendA b

--

-- class TooMany a where
--   tooMany :: a -> Bool

-- instance TooMany Int where
--   tooMany n = n > 42

-- newtype Goats = Goats Int deriving Show
-- instance TooMany Goats where
--   tooMany (Goats n) = n > 43

---

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

instance TooMany (Int, Int) where
  tooMany (n, n') = (n + n') > 42

instance (Ord a, Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, n') = (n + n') > 42

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

--

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction
  | NonfictionBook Nonfiction
  deriving Show

--

data Gardenia = Gardenia deriving Show
data Daisy = Daisy deriving Show
data Rose = Rose deriving Show
data Lilac = Lilac deriving Show

type Gardener = String

data Garden =
  GardeniaGarden Gardener Gardenia
  | DaisyGarden Gardener Daisy
  | RoseGarden Gardener Rose
  | LilacGarden Gardener Lilac
  deriving Show

--

-- newtype NumCow =
--   NumCow Int
--   deriving (Eq, Show)

-- newtype NumPig =
--   NumPig Int
--   deriving (Eq, Show)

-- data Farmhouse =
--   Farmhouse NumCow NumPig
--   deriving (Eq, Show)

-- type Farmhouse' = Product NumCow NumPig

-- ---

-- type Name = String
-- type Age = Int
-- type LovesMud = Bool
-- type PoundsOfWool = Int

-- data CowInfo =
--   CowInfo Name Age
--   deriving (Eq, Show)

-- data PigInfo =
--   PigInfo Name Age LovesMud
--   deriving (Eq, Show)

-- data SheepInfo =
--   SheepInfo Name Age PoundsOfWool
--   deriving (Eq, Show)

-- data Animal =
--   Cow CowInfo
--   | Pig PigInfo
--   | Sheep SheepInfo
--   deriving (Eq, Show)

-- type Animal' =
--   Sum CowInfo (Sum PigInfo SheepInfo)

-- bess' = (CowInfo "Bess" 4)
-- bess = First bess' :: Animal'

--

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

-- data Twitter =
--   Twitter deriving (Eq, Show)

-- data AskFm =
--   AskFm deriving (Eq, Show)

-- socialNetwork :: Sum Twitter AskFm
-- socialNetwork = First Twitter

-- type SN = Sum Twitter AskFm

-- snTest = First Twitter :: SN

data SocialNetwork =
  Twitter
  | AskFm
  deriving (Eq, Show)

type Twitter = String

type AskFm = String

twitter :: Sum Twitter AskFm
twitter = First "Twitter"

askfm :: Sum Twitter AskFm
askfm = First "AskFm"

--

data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
  , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer { os = os', lang = lang' }
  | os' <- allOperatingSystems, lang' <- allLanguages]

--

-- data ThereYet =
--   There Float Int Bool
--   deriving (Eq, Show)

-- notYet :: Int -> Bool -> ThereYet
-- notYet = nope 25.5

-- notQuite :: Bool -> ThereYet
-- notQuite = notYet 10

-- yusssss :: ThereYet
-- yusssss = notQuite False

--

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
  1
  (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
  2
  (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

--

preorder :: BinaryTree a -> [a]
preorder (Leaf)              = []
preorder (Node left a right) =
  [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder (Leaf)              = []
inorder (Node left a right) =
  (inorder left) ++ [a] ++ (inorder right)

-- 4 2 5 1 3

postorder :: BinaryTree a -> [a]
postorder (Leaf)              = []
postorder (Node left a right) =
  (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node
    (Node
      (Node Leaf 4 Leaf)
      2
      (Node Leaf 5 Leaf))
    1
    (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [1, 2, 4, 5, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [4, 2, 5, 1, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."


testPostorder :: IO ()
testPostorder =
  if postorder testTree == [4, 5, 2, 3, 1]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears."

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z (Leaf)              = z
foldTree f z (Node left a right) =
  (foldTree f (foldTree f (f a z) left) right)

--

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs@(x:xt) ys@(y:yt)
  | x == y      = isSubseqOf xt ys
  | otherwise   = isSubseqOf xs yt

testIsSubseqOf = all id [
  isSubseqOf "blah" "blahwoot" == True,
  isSubseqOf "blah" "wootblah" == True,
  isSubseqOf "blah" "wboloath" == True,
  isSubseqOf "blah" "wootbla" == False,
  isSubseqOf "blah" "halbwoot" == False,
  isSubseqOf "blah" "blawhoot" == True
  ]

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\w@(c:cs) -> (w, (toUpper c):cs)) . words

capitalizeWord :: String -> String
capitalizeWord (c:cs) =  (toUpper c):cs

data CapOrWaitForDot = Capitalize | WaitForDot

capitalizeParagraph :: String -> String
capitalizeParagraph = fst . foldl' capOrWaitForDot ("", Capitalize)
  where
    capOrWaitForDot :: (String, CapOrWaitForDot) -> Char -> (String, CapOrWaitForDot)
    capOrWaitForDot (str, Capitalize) c
      | isAlpha c                         = (str ++ [(toUpper c)], WaitForDot)
      | otherwise                         = (str ++ [(toUpper c)], Capitalize)
    capOrWaitForDot (str, WaitForDot) '.' = (str ++ ".", Capitalize)
    capOrWaitForDot (str, WaitForDot) c   = (str ++ [c], WaitForDot)

capitalizeParagraph' :: String -> String
capitalizeParagraph' = intercalate "." . map capitalizeFirstLetter . splitAtChar '.'

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter "" = ""
capitalizeFirstLetter (c:cs)
  | isAlpha c            = (toUpper c):cs
  | otherwise            = c : capitalizeFirstLetter cs

splitAtChar :: Char -> String -> [String]
splitAtChar _ "" = [""]
splitAtChar splitChar string = go [] string
  where
    go ws ""            = ws
    go ws (c:cs)
      | c == splitChar = go ws cs
    go ws cs            = go (ws ++ [takeWhile (/= splitChar) cs]) (dropWhile (/= splitChar) cs)

--

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

newtype DaPhone = DaPhone [Btn] deriving Show

data Btn = Btn Digit Chars deriving Show

type Chars = String

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

myPhone :: DaPhone
myPhone = DaPhone
  [ Btn '1' ""
  , Btn '2' "ABC"
  , Btn '3' "DEF"
  , Btn '4' "GHI"
  , Btn '5' "JKL"
  , Btn '6' "MNO"
  , Btn '7' "PQRS"
  , Btn '8' "TUV"
  , Btn '9' "WXYZ"
  , Btn '*' "^"
  , Btn '0' "+ "
  , Btn '#' ".,"
  ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone btns) char = maybe [] getBtnCharInfo btnMatch
  where
    getBtnCharInfo :: Btn -> [(Digit, Presses)]
    getBtnCharInfo (Btn d cs) =
      maybe [] (\index -> [(d, index + 1)]) $ elemIndex (toUpper char) cs

    btnMatch :: Maybe Btn
    btnMatch = find btnIncludesChar btns

    btnIncludesChar :: Btn -> Bool
    btnIncludesChar (Btn _ cs) = elem (toUpper char) cs

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = (>>= reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

mostPopularLetter :: String -> Char
mostPopularLetter =
  head
  . head
  . sortBy compareByLen
  . group
  . filter isAlpha
  . map toLower
  where
    compareByLen xs ys = compare (length xs) (length ys)

mostPopularWord :: String -> String
mostPopularWord =
  head
  . head
  . sortBy compareByLen
  . group
  . sort
  . map (filter isAlpha)
  . splitAtChar ' '
  . map toLower
  where
    compareByLen xs ys =
      case compare (length xs) (length ys) of
        EQ -> EQ
        LT -> GT
        GT -> LT


coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopularWord . intercalate " " . map mostPopularWord

--

a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit int) = int
eval (Add x y) = (eval x) + (eval y)

printExpr :: Expr -> String
printExpr (Lit int) = show int
printExpr (Add x y) = (printExpr x) ++ " + " ++ (printExpr y)

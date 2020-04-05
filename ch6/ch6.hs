import Data.List (sort)

class Numberish a where
  fromNumber :: Integer -> a
  toNumberRenamed :: a -> Integer
  defaultVal :: a


newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumberRenamed (Age n) = n
  defaultVal = Age 0

--

data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
  | Woot deriving (Show, Eq)

settleDown x = if x == Woot
  then Blah
  else x

type Subject = String
type Verb = String
type Object = String
data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
s3 = s1 "dogs"

--

type TestType = (String, Int)
newtype TestNewType = TestNewType (String, Int) deriving Eq
data TestData = TestData (String, Int) deriving Eq

newtype TestNewType2 = TestNewType2 Integer
-- testNewType2Sum = (TestNewType 1) + (TestNewType 2)

typesOfTypeTestX :: TestType
typesOfTypeTestX = ("A", 1)
typesOfTypeTestX1 = fst typesOfTypeTestX

typesOfTypeTestY :: TestNewType
typesOfTypeTestY = TestNewType ("A", 1)
-- y1 = fst y

typesOfTypeTestZ :: TestData
typesOfTypeTestZ = TestData ("A", 1)
-- z1 = fst z

--

type Rocks2 = String

type Yeah2 = Bool

data Papu2 =
  Papu2 Rocks2 Yeah2
  deriving (Eq, Show)

phew2 = Papu2  "chases" True

---

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

phew = Papu (Rocks "chases") (Yeah True)
truth = Papu (Rocks "chomskydoz")
  (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

---

i :: Num a => a
-- i :: a
i = 1

-- f :: Float
-- f :: Fractional a => a
-- f = 1.0

--

-- f :: Float
f :: RealFrac a => a
f = 1.0

freud :: a -> a
-- freud :: Ord a => a -> a
freud x = x

-- myX = 1 :: Int
-- sigmund :: Int -> Int
sigmund :: a -> a
sigmund x = freud x

myX = 1 :: Int
-- b) sigmund' :: Num a => a -> a
sigmund' :: Int -> Int
sigmund' x = myX

-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b =  (aToB a) == b

chkRes = chk (\x -> if x == 1 then True else False) 2 True

arith :: Num b
  => (a -> b)
  -> Integer
  -> a
  -> b
arith aToB int a = (fromInteger int) + (aToB a)

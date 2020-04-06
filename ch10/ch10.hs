import Data.Time
import Data.List

test :: String
test = foldr (\x y -> max [x] y) "" "fear is the little death"

test1 = foldr (&&) True [False, True]

test2 = foldl (\x y -> (x ++) $ show y) "" [1..5]

test3 = foldr (flip const) 'a' [1..5]

--

data DatabaseItem =
  DbString   String
  | DbNumber Integer
  | DbDate   UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
  (fromGregorian 1921 5 1)
  (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map deconsDbdate . filter isDbDate
  where
    isDbDate (DbDate utcTime) = True
    isDbDate _              = False
    deconsDbdate (DbDate utcTime) = utcTime

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent =  (\ds -> foldr max (head ds) ds) . filterDbDate

factorial 0 = 1
factorial n = factorial (n - 1) * n

factorialViaScanl :: Integral a => [a]
factorialViaScanl = scanl (*) 1 [2..]

takeFactorial :: Integral a => Int -> [a]
takeFactorial = flip take $ factorialViaScanl

---

stops = "pbtdkg"
vowels = "aeiou"
svs = [(s, v) | s <- stops, v <-vowels]
svss = [(s, v, s') | (s, v) <- svs, s' <- stops]

seekritFunc x =
  (/) (fromIntegral $ sum $ map length $ words x)
      (fromIntegral $ length $ words x)
---

myOr :: [Bool] -> Bool
myOr = foldl' (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny a2Bool = foldl' (||) False . map a2Bool

myMap :: (a -> b) -> [a] -> [b]
myMap a2b = foldr (\a bs -> (a2b a):bs) []

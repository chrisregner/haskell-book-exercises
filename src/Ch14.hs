module Ch14 where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

myProduct :: (Ord a, Num a) => a -> a -> a
myProduct 0 _    = 0
myProduct _ 0    = 0
myProduct 1 y    = y
myProduct x 1    = x
myProduct (-1) y = negate y
myProduct x (-1) = negate x
myProduct x y
  | y > 0 = x + (myProduct x $ y - 1)
  | y < 0 = (-x) + (myProduct x $ y + 1)

main :: IO ()
main = hspec $ do
    describe "myProduct" $ do
        it "3 * 4 = 12" $
            myProduct 3 4 `shouldBe` 12
        it "3 * 1 = 3" $
            myProduct 3 1 `shouldBe` 3
        it "0 * 3 = 0" $
            myProduct 0 1 `shouldBe` 0
        it "3 * -4 = -12" $
            myProduct 3 (-4) `shouldBe` (-12)

---

half :: Fractional a => a -> a
half x = x / 2

runQc :: IO ()
runQc = hspec $ do
    describe "half" $ do
        it "half x multiplied by two always equals x" $ do
            property $ \x -> (half x) * 2 == (x :: Double)

    describe "sort" $ do
        it "sorted list should have elements \
            \that is greater than its predecessor" $ do
            property $ \xs -> listOrdered . sort $ (xs :: [Int])

    describe "(+)" $ do
        it "holds associative property" $ do
            property $ (associative (+) :: Int -> Int -> Int -> Bool)
        it "holds commutative property" $ do
            property $ (commutative (+) :: Int -> Int -> Bool)

    describe "(*)" $ do
        it "holds associative property" $ do
            property $ (associative (*) :: Int -> Int -> Int -> Bool)
        it "holds commutative property" $ do
            property $ (commutative (*) :: Int -> Int -> Bool)

    -- describe "(^)" $ do
    --     it "holds associative property" $ do
    --         property $ (associative (^) :: Int -> Int -> Int -> Bool)
    --     it "holds commutative property" $ do
    --         property $ (commutative (^) :: Int -> Int -> Bool)

    describe "quot and rem" $ do
        it "(quot x y)*y + (rem x y) == x" $ do
            property $
                \(MyNonZero x) (MyNonZero y) ->
                    (quot x y)*y + (rem x y) == (x :: Integer)
            -- property $ forAll genNonZeroTuple $
            --     \(MyNonZero x, MyNonZero y) ->
            --         (quot x y)*y + (rem x y) == (x :: Integer)

    describe "div and mod" $ do
        it "(div x y)*y + (mod x y) == x" $ do
            property $
                \(MyNonZero x) (MyNonZero y) ->
                    (div x y)*y + (mod x y) == (x :: Integer)

    describe "reverse" $ do
        it "list reversed twice is equal to the list" $ do
            property $
                \xs -> (reverse $ reverse xs) == (xs :: [Integer])

    describe "($)" $ do
        it "(f $ g) x is same as f (g x)" $ do
            let prop :: Fun Int Int -> Fun Int Int -> Int -> Bool
                prop (Fun _ f) (Fun _ g) x = (f $ g x) == (f (g x))
            property $ prop

    describe "(.)" $ do
        it "(f . g) x is same as f (g x)" $ do
            let prop :: Fun Int Int -> Fun Int Int -> Int -> Bool
                prop (Fun _ f) (Fun _ g) x = ((f . g) x) == (f (g x))
            property $ prop

    describe "foldr" $ do
        it "foldr (++) [] == concat" $ do
            property $ \xss ->
                (foldr (++) [] xss) == (concat (xss :: [[Int]]))

    describe "length and take" $ do
        it "f n xs = length (take n xs) == n" $ do
            property $ forAll genListAndSublength $
                \(xs, n) -> length (take n xs) == (n :: Int)

    describe "read and show" $ do
        it "f x = (read (show x)) == x" $ do
            property $ \x ->  (read (show x)) == (x :: String)

    -- describe "(square)" $ do
    --     it "x is equal to square of square root of x" $ do
    --         property $ \x -> (square . sqrt $ x) == (square (x :: Double))

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
  where
    go :: (Ord a) => a -> (Maybe a, Bool) -> (Maybe a, Bool)
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

associative f x y z = x `f` (y `f` z) == (x `f` y) `f` z
commutative f x y = x `f` y == y `f` x

newtype MyNonZero a = MyNonZero a deriving (Eq, Show)

instance (Eq a, Num a, Arbitrary a) => Arbitrary (MyNonZero a) where
    arbitrary = genNonZero

genNonZero :: (Eq a, Num a, Arbitrary a) => Gen (MyNonZero a)
genNonZero = MyNonZero <$> arbitrary `suchThat` (/= 0)
-- genNonZero = do
--     x <- arbitrary `suchThat` (/= 0)
--     return $ MyNonZero x

genNonZeroTuple :: (Eq a, Num a, Arbitrary a) => Gen (MyNonZero a , MyNonZero a)
genNonZeroTuple = do
    x <- arbitrary `suchThat` (/= 0)
    y <- arbitrary `suchThat` (/= 0)
    return (MyNonZero x, MyNonZero y)


genListAndSublength :: Gen ([Int], Int)
genListAndSublength = do
    xs <- arbitrary
    n <- arbitrary `suchThat` \n' -> n' <= length xs && n' >= 0
    return (xs, n)

square x = x * x

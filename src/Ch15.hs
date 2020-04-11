module Ch15 where

import Data.Monoid
import Test.QuickCheck

data Optional a
    = Nada
    | Only a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    Nada <> (Only x) = Only x
    (Only x) <> Nada = Only x
    (Only x) <> (Only y) = Only $ x <> y

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

--

newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        x <- arbitrary
        frequency
            [ (1, return $ First' $ Only x)
            , (1, return $ First' Nada)
            ]

instance Semigroup (First' a) where
    (First' Nada) <> (First' Nada) = First' Nada
    (First' (Only x)) <> _ = First' (Only x)
    _ <> (First' (Only y)) = First' (Only y)

instance Monoid (First' a) where
    mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)

--

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
    Combine aTob <> Combine aToB' = Combine $ \a -> (aTob a) <> (aToB' a)

--

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
    (Mem x) <> (Mem y) = Mem $ \s ->
        ( (fst $ x s) <> (fst $ y s)
        , (snd . x . snd . y $ s)
        )

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)

f' = Mem $ \s -> ("hi", s + 1)
f'' = Mem $ \s -> ("hello", s + 10)

mainMem = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft -- ("hi",1)
    print $ rmright -- ("hi",1)
    print $ (rmzero :: (String, Int)) -- ("",0)
    print $ rmleft == runMem f' 0 -- True
    print $ rmright == runMem f' 0 -- True

{-# LANGUAGE FlexibleInstances #-}

module Ch16 where

import GHC.Arr
import Test.QuickCheck
import Test.QuickCheck.Function

-- >>> d 0
-- "1[0,1,2,3]"
--
d =
    fmap ((return '1' ++) . show)
    $ (\x -> [x, 1..3])

-- >>> e
-- 3693
--
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read <$> ("123"++) <$> show <$> ioi
    in (*3) <$> changed

---

newtype Identity a = Identity a deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

--

data Pair a = Pair a a deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        return $ Pair a a'

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

--

data Two a b = Two a b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

--

data Three a b c = Three a b c deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

--

data Three' a b = Three' a b b

data Four a b c d = Four a b c d

data Four' a b = Four' a a a b

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

fi :: (Functor f, Eq (f a)) => f a -> Bool
fi = functorIdentity

fc :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
fc = functorCompose'

runQc :: IO ()
runQc = do
        quickCheck (fi :: Identity Int -> Bool)
        quickCheck (fc :: Identity Int -> IntToInt -> IntToInt -> Bool)

        quickCheck (fi :: Pair Int -> Bool)
        quickCheck (fc :: Pair Int -> IntToInt -> IntToInt -> Bool)

        quickCheck (fi :: Two Int Int -> Bool)
        quickCheck (fc :: Two Int Int -> IntToInt -> IntToInt -> Bool)

        quickCheck (fi :: Three Int Int Int -> Bool)
        quickCheck (fc :: Three Int Int Int -> IntToInt -> IntToInt -> Bool)

data Sum a b
    = First b
    | Second a

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b

data Company a b c
    = DeepBlue a b
    | Something c

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

data More a b
    = L b a b
    | R a b a
    deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

--

newtype Flip f a b
    = Flip (f b a)
    deriving (Eq, Show)

newtype K a b = K a

instance Functor (K a) where
    fmap f (K a)= K a

instance Functor (Flip K a) where
    fmap f (Flip (K b))= Flip $ K (f b)

--

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

--

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

--

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

--

data List a
    = Nil
    | Cons a (List a)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a l) = Cons (f a) $ fmap f l

--

data TalkToMe a
    = Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap f Halt = Halt
    fmap f (Print str a) = Print str $ f a
    fmap f (Read strToA) = Read $ fmap f strToA

unwrapRead (Read strToA) str = strToA str

-- >>> r = (Read (\str -> 1))
-- >>> unwrapRead (fmap (+10) r) "test"
-- 11
--

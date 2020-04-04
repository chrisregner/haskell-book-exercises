bigNum :: Num a => a
bigNum = (^) 5 $ 10

-- wahoo = bigNum $ 10

functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

i :: a -> a
i a = a

c :: a -> b -> a
c x _ = x

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB x = bToC (aToB x)

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' aToB = aToB

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g (f x)

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xToY yToWZ x = fst (yToWZ (xToY x))

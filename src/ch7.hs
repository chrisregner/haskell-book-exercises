isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

fn :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
fn (a, _, c) (d, _, f) = ((a, d), (c, f))

pal xs
  | xs == reverse xs = True
  | otherwise        = False

numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit1 :: Integral a => a -> a
tensDigit1 x = d
  where (xLast, _) = divMod x 10
        (_, d) = divMod xLast 10

hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where xLast = x `div` 100
        d     = xLast `mod` 10

foldBoolViaCase :: a -> a -> Bool -> a
foldBoolViaCase a a' bool =
  case bool of
    True -> a'
    False -> a

foldBoolViaGuard :: a -> a -> Bool -> a
foldBoolViaGuard a a' bool
  | bool = a'
  | otherwise = a

g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = ((aToB a), c)

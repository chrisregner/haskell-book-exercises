sumTil :: (Ord a, Num a) => a -> a
sumTil 1 = 1
sumTil x
  | x <= 0    = 0
  | otherwise = x + sumTil (x - 1)

mc91 :: (Ord a, Num a) => a -> a
mc91 a
  | a > 100 = a - 10
  | otherwise = 91

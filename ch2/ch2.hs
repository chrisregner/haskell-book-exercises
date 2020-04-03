sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")

half :: Fractional a => a -> a
half x = x / 2

square :: Num a => a -> a
square x = x * x

circleArea :: Floating a => a -> a
circleArea x = pi * square(x)

parenAndAssocTest :: Double -> Double -> (Bool, Bool, Bool)
parenAndAssocTest v1 v2 =
  (
    8 + 7 * 9 /= (8 + 7) * 9,
    ((\x y -> (x * 2) + (y * 2)) v1 v2) == ((\x y -> x * 2 + y * 2) v1 v2),
    ((\x -> x / 2 + 9) v1) /= ((\x -> x / (2 + 9)) v1)
  )

-- stack config set resolver ghc-8.6.4

area x = 3.14 * (x * x)

double x = x * 2

x = 7
y = 10
f = x + y

headCodeTest =
  (
    (let x = 5 in x) == 5,
    (let x = 5 in x * x) == 25,
    (let x = 5; y = 6 in x * y) == 30,
    (let x = 3; y = 1000 in x + 3) == 6
  )

headCodeWhereItemTest1 = x where x = 5
headCodeWhereItemTest2 = x * x where x = 5
headCodeWhereItemTest3 = x * y where x = 5; y = 6
headCodeWhereItemTest4 = x + 3 where x = 3; y = 1000

headCodeWhereTest =
  (
    headCodeWhereItemTest1 == 5,
    headCodeWhereItemTest2 == 25,
    headCodeWhereItemTest3 == 30,
    headCodeWhereItemTest4 == 6
  )

parenTest =
  (
    (2 + 2 * 3 - 1) == ((2 + (2 * 3)) - 1),
    ((^) 10 $ 1 + 1) == (((^) 10) $ (1 + 1)),
    (2 ^ 2 * 4 ^ 5 + 1) == (((2 ^ 2) * (4 ^ 5)) + 1)
  )

equivExpTest =
  (
    ((1 + 1 == 2) == True),
    (((10 ^ 2) == 10 + 9 * 10) == True),
    ((400 - 37 == (-) 37 400) == False),
    (((100 `div` 3) == 33) == True),
    (100 / 3),
    (((2 * 5 + 18) == 2 * (5 + 18)) == False)
  )

waxOn = x * 5 where
  z = 7
  x = y ^ 2
  y = z + 8

waxOnTest =
  (
    (10 + waxOn) == 1135,
    ((+10) waxOn) == 1135,
    ((-) 15 waxOn) == (-1110),
    ((-) waxOn 15) == 1110
  )

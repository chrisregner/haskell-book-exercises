-- import Prelude

-- Exercise: Scope
-- yes
-- no
-- no
-- yes


-- Sandbox: infixl and infixr
-- (>>>>) :: Int -> Int -> Int
-- a >>>> b = a - b

-- infixl 6 >>>>

-- (<<<<) :: Int -> Int -> Int
-- a <<<< b = a - b

rvrs :: String -> String
rvrs str =
  -- Curry is awesome
  (drop 9 str) ++ (take 4 (drop 5 str)) ++ (take 5 str)

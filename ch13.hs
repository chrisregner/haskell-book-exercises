import Data.Char (toLower, isAlpha)
import Text.Read (readMaybe)
import Control.Monad (forever)
import System.Exit (exitSuccess)

main :: IO ()
main = forever $ do
    line1 <- getLine
    case palindrome line1 of
        True -> putStrLn "It's a palindrome!"
        False -> do
            putStrLn "Nope!"
            exitSuccess

-- >>> palindrome "Madam I’m Adam,"
palindrome :: String -> Bool
palindrome str = (simplify str) == (reverse $ simplify str)
  where
    simplify :: String -> String
    simplify = map toLower . filter isAlpha

--

main2 :: IO ()
main2 = forever $ do
    putStrLn "What is the person's name?"
    name <- getLine
    putStrLn "What is the person's age?"
    ageStr <- getLine
    let ageM = readMaybe ageStr :: Maybe Integer
    case ageM of
        Nothing -> putStrLn "Invalid Age"
        Just age -> case mkPerson name age of
            Left err -> putStrLn $ show err
            Right person -> putStrLn
                $ ("Yay! Successfully got a person: " ++)
                $ show person

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid
    = NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson
    :: Name
    -> Age
    -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

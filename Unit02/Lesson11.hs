module Lesson11 where

--
-- Types in Haskell
--
--
x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1, 2, 3]

testScores :: [Double]
testScores = [0.99, 0.7, 0.8]

letters :: String
letters = ['a', 'b', 'c']

ageAndHeight :: (Int, Int)
ageAndHeight = (34, 74)

firstLastMiddle :: (String, String, Char)
firstLastMiddle = ("Oscar", "Grouch", 'D')

streetAddress :: (Int, String)
streetAddress = (123, "Happy St.")

--
-- Function types
--
--
half :: Int -> Double
half n = fromIntegral n / 2

-- QC1101
halve :: Int -> Int
halve n = n `div` 2

-- QC1102
-- E.g. printDouble 2 == "4"
printDouble :: Int -> String
printDouble n = show (n * 2)

anotherNumber = read "6" :: Double

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

-- QC1103
--
makeAddress' = makeAddress 123 :: String -> String -> (Int, String, String)

makeAddress'' = makeAddress 123 "Main" :: String -> (Int, String, String)

makeAddress''' = makeAddress 123 "Main" "Rochester" :: (Int, String, String)

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n =
  if even n
    then f n
    else n

--
-- Type variables
--
--
simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)

-- QC1104
-- Because the function argument supplied to map can return a type that is different
-- from the type of its argument
--
-- Q1101
-- filter :: (a -> Bool) -> [a] -> [a]
-- map    :: (a -> b)    -> [a] -> [b]
-- The function supplied to `filter` is required to return Bool, while the function
-- supplied to `map` can return any type. Also, `filter` takes and returns the same type
-- of list, while `map' can return a list of different type.
--
-- Q1102
-- head :: [a] -> a
-- tail :: [a] -> [a]
-- No, because `head` doesn't return a list.
--
-- Q1103
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where
    newInit = f init x

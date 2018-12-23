module Lesson11 where

--
-- Consider this:
--

-- Q: Why doesn't this work?
-- average' xs = sum xs / length xs
--  -> "Could not deduce (Fractional Int) arising from a use of ‘/’"

-- A: Because `/` expects a Fractional while `length` produces an Int
--  -> Use `fromIntegral` to convert the result of `length` to a Num
--     which can be used with `/`
{-
average' [2,3,4] -- 3.0
-}
average' xs = sum xs / fromIntegral (length xs)

--
-- Types in Haskell
--

x :: Int
x = 2

y :: Integer
y = 2

-- Difference between Int and Integer?
{-
x ^ 2000 -- 0 (exceeds upper bound of Int as required by computer architecture)
y ^ 2000 -- 11481... (+ 598 more digits - there is no upper bound for Integer)
-}

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

double :: Int -> Int
double n = n * 2

half :: Int -> Double
half n = fromIntegral n / 2

-- QC1

{-
halve 5 -- 2
-}
halve :: Int -> Int
halve n = n `div` 2

-- show

{-
show 6   -- "6"
show 'c' -- "'c'"
show 6.0 -- "6.0"
-}

-- QC2

{-
printDouble 2 -- "4"
-}
printDouble :: Int -> String
printDouble n = show (n * 2)

-- read - usually requires a type annotation

anotherNumber :: Double
anotherNumber = read "6" -- 6.0

-- can also put the type at the end:
{-
read "6" :: Int    -- 6
read "6" :: Double -- 6.0
-}

-- Functions with multiple arguments

{-
makeAddress 123 "Happy St." "Haskell Town" -- (123,"Happy St.","Haskell Town")
-}
makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

-- Equivalent:
{-
(((makeAddressLambda 123) "Happy St.") "Haskell Town") -- (123,"Happy St.","Haskell Town")
(((makeAddress 123) "Happy St.") "Haskell Town")       -- (123,"Happy St.","Haskell Town")
-}
makeAddressLambda :: Int -> String -> String -> (Int, String, String)
makeAddressLambda = (\number -> (\street -> (\town -> (number, street, town))))

-- QC3
makeAddress' = makeAddress 123 :: String -> String -> (Int, String, String)
makeAddress'' = makeAddress 123 "Main" :: String -> (Int, String, String)
makeAddress''' = makeAddress 123 "Main" "Rochester" :: (Int, String, String)

-- Types for first-class functions

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n then f n else n

--
-- Type variables
--

simple :: a -> a
simple x = x

{-
makeTriple "Oscar" 'D' "Grouch" -- ("Oscar",'D',"Grouch") :: (String, Char, String)
-}
makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)

-- QC4
-- Because the function argument supplied to map can return a type that is different
-- from the type of its argument.

--
-- Summary
--

-- Q1
-- filter :: (a -> Bool) -> [a] -> [a]
-- map    :: (a -> b)    -> [a] -> [b]
-- The function supplied to `filter` is required to return Bool, while the function
-- supplied to `map` can return any type. Also, `filter` takes and returns the same type
-- of list, while `map' can return a list of any type.

-- Q2
-- head :: [a] -> a
-- tail :: [a] -> [a]
-- No, because `head` doesn't return a list.

-- Q3

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init []       = init
myFoldl f init (x : xs) = myFoldl f newInit xs where newInit = f init x

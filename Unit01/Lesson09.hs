module Lesson09 where

import Data.Char

--
-- Using map
--

-- Add the indefinite article to the beginning of each word
-- E.g. determine ["dog","cat","moose"] == ["a dog","a cat","a moose"]
determine = map ("a "++)

--
-- Abstracting away recursion with map
--

addAnA []     = []
addAnA (x:xs) = ("a " ++ x) : addAnA xs

squareAll []     = []
squareAll (x:xs) = x^2 : squareAll xs

-- Generalize the function to `f`:

map' f []     = []
map' f (x:xs) = f x : map' f xs

--
-- Filtering a list
--

-- filter even [1,2,3,4] == [2,4]
-- filter (\(x:xs) -> x == 'a') ["apple","banana","avocado"] == ["apple","avocado"]

filter' p []     = []
filter' p (x:xs) = if p x
                   then x : filter' p xs
                   else     filter' p xs

-- QC0901

-- E.g. remove (> 3) [1,2,3,4,5] == [1,2,3]
remove p []     = []
remove p (x:xs) = if p x
                  then     remove p xs
                  else x : remove p xs

--
-- Folding a list
--

-- QC0902
product' xs = foldl (*) 1 xs

concat' xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^2) xs)

reverse' xs = foldl (\x y -> y : x) [] xs

foldl' f init []     = init
foldl' f init (x:xs) = foldl' f (f init x) xs

foldr' f init []     = init
foldr' f init (x:xs) = f x (foldr f init xs)

-- Q0901
elem' a xs = length (filter (== a) xs) > 0

-- Q0902
isPalindrome xs = xs' == reverse xs'
                  where xs' = map toUpper $ filter (/= ' ') xs

-- Q0903
-- Cheat!
-- Interesting - it's divergent:
--    harmonic 10   == 2.9289682539682538
--    harmonic 100  == 5.187377517639621
--    harmonic 1000 == 7.485470860550343
harmonic n = sum (take n seriesValues)
             where seriesPairs = zip (repeat 1.0)  [1.0,2.0..]
                   seriesValues = map (\pair -> fst pair / snd pair) seriesPairs

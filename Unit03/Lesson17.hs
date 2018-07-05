module Lesson17 where

import           Data.List
import           Data.Semigroup                 ( Semigroup
                                                , (<>)
                                                )

-- Consider this

-- Can we do better?
ct1 :: String
ct1 = "this" ++ " " ++ "is" ++ " " ++ "a" ++ " " ++ "bit" ++ " " ++ "much"

--
-- Intro to composability - combining functions
--

{-
last' [1, 2, 3] -- 3
-}
last' :: [a] -> a
last' = head . reverse

{-
minimum' "electric" -- 'c'
-}
minimum' :: Ord a => [a] -> a
minimum' = head . sort

{-
maximum' "electric" -- 't'
-}
maximum' :: Ord a => [a] -> a
maximum' = last' . sort

{-
all' (> 2) [3, 4, 5] -- True
all' (> 2) [2, 3, 4] -- False
-}
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p -- `and` returns whether all of a list of Bools are True

-- QC1

{-
any' (< 3) [2, 4, 6] -- True
-}
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p -- `or` returns whether any of a list of Bools is True

--
-- Combining like types: Semigroups
--

{-
2 <> 3 -- 5
-}
instance Semigroup Integer where
  (<>) x y = x + y

-- QC2
-- No, because division can return a Double.

--

data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  | Transparent
  deriving (Show, Eq)

-- Guards

howMuch :: Int -> String
howMuch n | n > 10    = "a whole bunch"
          | n > 0     = "not much"
          | otherwise = "we're in debt!"

{-
Red <> Yellow   -- Orange
Red <> Blue     -- Purple
Green <> Purple -- Brown
-}
instance Semigroup Color where
  Transparent <> c = c
  c <> Transparent = c
  Red <> Blue = Purple
  Blue <> Red = Purple
  Yellow <> Blue = Green
  Blue <> Yellow = Green
  Yellow <> Red = Orange
  Red <> Yellow = Orange
  a <> b | a == b = a
         | all (`elem` [Red, Blue, Purple]) [a, b]   = Purple
         | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
         | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
         | otherwise = Brown

-- QC3
-- Yes, because addition is associative.

-- Semigroup law
{-
Associativity:
x <> (y <> z) == (x <> y) <> z
-}

--
-- Composing with identity: Monoids
--

-- Conceptually:
{-
class Semigroup a => Monoid a where
  identity :: a
-}

-- Actually:
{-
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
-}

-- These are equivalent:

eg1 = [1, 2, 3] ++ []

eg2 = [1, 2, 3] <> []

eg3 = [1, 2, 3] `mappend` mempty


-- QC4
-- 1

-- mconcat = foldr mappend mempty
-- mconcat ["a","b","cde"] == "abcde"

-- Monoid laws
{-
mappend mempty x == x
mappend x mempty == x
mappend x (mappend y z) == mappend (mappend x y) z
mconcat == foldr mappend mempty
-}

-- Practical Monoids - building probability tables

type Events = [String]

type Probs = [Double]

data PTable =
  PTable Events
         Probs

-- Create a probability table, ensuring all probabilities sum to 1 by dividing
-- all the probabilities by the sum of the probabilities
createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
 where
  totalProbs      = sum probs
  normalizedProbs = map (/ totalProbs) probs

-- Print a single table row
showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where
      pairs = zipWith showPair events probs

-- Generate all combinations of two lists using the specified function `f`
-- E.g. cartesianCombine (\x y -> mconcat [x, "-", y]) ["red", "blue"] ["red", "blue"]
--        == ["red-red","red-blue","blue-red","blue-blue"]
--      cartesianCombine (*) [2,3,4] [5,6] == [10,12,15,18,20,24]
cartesianCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianCombine f l1 l2 = zipWith f newL1 cycledL2
 where
  nToAdd     = length l2
  repeatedL1 = map (replicate nToAdd) l1
  newL1      = mconcat repeatedL1
  cycledL2   = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents = cartesianCombine (\x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs = cartesianCombine (*)

instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

-- Example PTables

coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

--

-- The <> operator gives us the probability of each possible combo:
{-
coin <> spinner ==
  heads-red|5.0e-2
  heads-blue|0.1
  heads-green|0.35
  tails-red|5.0e-2
  tails-blue|0.1
  tails-green|0.35
-}

-- Probability of flipping heads three times in a row:
{-
mconcat [coin,coin,coin] ==
  heads-heads-heads|0.125
  heads-heads-tails|0.125
  heads-tails-heads|0.125
  heads-tails-tails|0.125
  tails-heads-heads|0.125
  tails-heads-tails|0.125
  tails-tails-heads|0.125
  tails-tails-tails|0.125
-}

-- Q1

instance Monoid Color where
  mempty = Transparent
  mappend = (<>)

-- Q2
-- See Lesson17_Q2.hs

module Lesson17_Q2 where

import           Data.Semigroup

newtype Events =
  Events [String]

newtype Probs =
  Probs [Double]

data PTable =
  PTable Events
         Probs

-- Create a probability table, ensuring all probabilities sum to 1 by dividing
-- all the probabilities by the sum of the probabilities
createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events) (Probs normalizedProbs)
  where
    totalProbs = sum probs
    normalizedProbs = map (/ totalProbs) probs

-- Print a single table row
showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat pairs
    where
      pairs = zipWith showPair events probs

-- Generate all combinations of two lists using the specified function `f`
-- E.g. cartesianCombine (\x y -> mconcat [x, "-", y]) ["red", "blue"] ["red", "blue"]
--        == ["red-red","red-blue","blue-red","blue-blue"]
--      cartesianCombine (*) [2,3,4] [5,6] == [10,12,15,18,20,24]
cartesianCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianCombine f l1 l2 = zipWith f newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (replicate nToAdd) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartesianCombine (\x y -> mconcat [x, "-", y]) e1 e2)

instance Semigroup Events where
  (<>) = combineEvents

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartesianCombine (*) p1 p2)

instance Semigroup Probs where
  (<>) = combineProbs

instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)

instance Semigroup PTable where
  (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
  (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable (e1 <> e2) (p1 <> p2)

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

--
-- Example PTables
--
coin = createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])

spinner = createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])
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
--
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

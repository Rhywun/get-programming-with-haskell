module Lesson17 where

import           Data.List
import           Data.Semigroup

--
--
-- Intro to composability - combining functions
--
--
last' :: [a] -> a
last' = head . reverse

minimum' :: Ord a => [a] -> a
minimum' = head . sort

maximum' :: Ord a => [a] -> a
maximum' = last' . sort

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p -- `and` returns whether all Bools are True

-- QC1701
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p -- `or` returns whether any of the Bools is True

--
--
-- Combining like types: Semigroups
--
--
instance Semigroup Integer where
  (<>) x y = x + y

--
-- QC1702
-- No, because division can return a Double.
--
--
data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  deriving (Show, Eq)

--
-- Guards
--
howMuch :: Int -> String
howMuch n
  | n > 10 = "a whole bunch"
  | n > 0 = "not much"
  | otherwise = "we're in debt!"

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
    | otherwise = Brown
--
-- QC1703
-- Yes, because addition is associative.
--
-- Semigroup law
{-
x <> (y <> z) == (x <> y) <> z
-}
--
-- Composing with identity: Monoids
--
--
-- Conceptually:
{-
class Semigroup a => Monoid a where
  identity :: a
-}
--
-- Actually:
{-
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
-}
--
-- These are equivalent:
eg1 = [1,2,3] ++ []
eg2 = [1,2,3] <> []
eg3 = [1,2,3] `mappend` mempty
--
-- QC1704
-- 1

--
-- mconcat = foldr mappend mempty
-- mconcat ["a","b","cde"] == "abcde"

--
-- Monoid laws
{-
mappend mempty x == x
mappend x mempty == x
mappend x (mappend y z) == mappend (mappend x y) z
mconcat == foldr mappend mempty
-}
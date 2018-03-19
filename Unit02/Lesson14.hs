module Lesson14 where

import           Data.List

data NewEngland
  = CT
  | MA
  | ME
  | NH
  | RI
  | VT

instance Show NewEngland where
  show x =
    case x of
      CT -> "Connecticut"
      MA -> "Massachussetts"
      ME -> "Maine"
      NH -> "New Hampshire"
      RI -> "Rhode Island"
      VT -> "Vermont"

--
--
-- A type in need of classes;
-- Implementing Show;
-- Default implementation and minimum complete definitions;
-- Implementing Ord;
-- To derive or not to derive?
--
--
data SixSidedDie
  = S1
  | S2
  | S3
  | S4
  | S5
  | S6
  deriving (Eq, Ord, Enum)

instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

--
-- Q1402
-- RealFrac's minimal complete definition is `properFraction`.
--
-- NOTE: the space after `S1` is required here:
-- [S1 ..] == [one,two,three,four,five,six]
--
--
-- Type classes for more-complex types
--
--
newtype Name -- was "data" but hlint suggests "newtype"
         =
  Name (String, String)
  deriving (Show, Eq)

names =
  [ Name ("Emil", "Cioran")
  , Name ("Eugene", "Thacker")
  , Name ("Friedrich", "Nietzsche")
  ] :: [Name]

-- Sort by last name
instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

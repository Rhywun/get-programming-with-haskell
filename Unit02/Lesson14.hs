module Lesson14 where

import           Data.List

-- Consider this

data NewEngland = CT | MA | ME | NH | RI | VT

instance Show NewEngland where
  show x = case x of CT -> "Connecticut"
                     MA -> "Massachussetts"
                     ME -> "Maine"
                     NH -> "New Hampshire"
                     RI -> "Rhode Island"
                     VT -> "Vermont"

--
-- A type in need of classes
--

data SixSidedDie' = S1' | S2' | S3' | S4' | S5' | S6'

--
-- Implementing Show
--

instance Show SixSidedDie' where
  show S1' = "one"
  show S2' = "two"
  show S3' = "three"
  show S4' = "four"
  show S5' = "five"
  show S6' = "six"

--
-- Type classes and polymorphism
--

poly1 :: Int
poly1 = read "10" :: Int -- 10

poly2 :: Double
poly2 = read "10" :: Double -- 10.0

--
-- Default implementation and minimum complete definitions
--

-- Notice we don't have to implement (/=):
instance Eq SixSidedDie' where
  (==) S6' S6' = True
  (==) S5' S5' = True
  (==) S4' S4' = True
  (==) S3' S3' = True
  (==) S2' S2' = True
  (==) S1' S1' = True
  (==) _ _   = False

-- Of course, this is the same as deriving (Eq)

-- QC2
-- RealFrac's minimal complete definition is `properFraction`.

--
-- Implementing Ord
--

instance Ord SixSidedDie' where
  compare S6' S6' = EQ
  compare S6' _   = GT
  compare _ S6'   = LT
  compare S5' S5' = EQ
  compare S5' _   = GT
  compare _ S5'   = LT
  compare _ _   = undefined -- added to let it compile; in reality
                            -- this needs many more cases

--
-- To derive or not to derive?
--

-- At the end of the day, we're better off using `deriving` whenever possible

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show, Eq, Ord, Enum)

-- Now we can take advantage of Enum too:
{-
[S1 ..] -- [S1,S2,S3,S4,S5,S6]
-}

--
-- Type classes for more-complex types
--

-- see text for discussion of `newtype`
newtype Name = Name (String, String) deriving (Show, Eq)

names :: [Name]
names =
  [Name ("Emil", "Cioran"), Name ("Eugene", "Thacker"), Name ("Friedrich", "Nietzsche")]

-- Sort by last name:
instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

{-
sort names -->
[Name ("Emil","Cioran"),Name ("Friedrich","Nietzsche"),Name ("Eugene","Thacker")]
-}

--
-- Summary
--

-- Q1

data Boo = Tru | Fls deriving (Enum)

instance Eq Boo where
  x == y = fromEnum x == fromEnum y

instance Ord Boo where
  compare x y = compare (fromEnum x) (fromEnum y)

-- Q2
{-
class Integral a => Die a where
  sides :: a
  roll :: a -> a

newtype FiveSidedDie = D5 Int deriving Show

instance Die FiveSidedDie where
  sides = 5

-- PASS
-- I'm trying to come up with a class Die that holds the number of sides so I can
-- have subclasses that don't look like S1 | S2 | etc. etc. but I can't figure it out.
-}

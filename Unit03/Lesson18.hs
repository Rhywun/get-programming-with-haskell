module Lesson18 where

import qualified Data.Map as Map

--
--
-- Types that take arguments
--
--
--
-- Simplest parameterized type:
--
newtype Box a =
  Box a
  deriving (Show)

wrap :: a -> Box a
wrap = Box

unwrap :: Box a -> a
unwrap (Box x) = x

--
-- QC1801
-- :t wrap (Box 'a') --> Box (Box Char)
--
--
data Triple a =
  Triple a
         a
         a
  deriving (Show)

type Point3D = Triple Double

aPoint = Triple 0.1 53.2 12.3 :: Point3D

type FullName = Triple String

aPerson = Triple "Howard" "Phillips" "Lovecraft"

--
-- Accessors
--
first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

--
--
toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

-- Hm. This looks like a functor?
transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

--
--
-- QC1802
-- `map` can return a list with a different type from the original; `transform` can't.
--
--
-- Implement my own List type
-- i.e. data [] a = [] | a:[a]
--
data List a
  = Empty
  | Cons a
         (List a)
  deriving (Show)

list1 = Cons 1 (Cons 2 (Cons 3 Empty)) :: List Int

list2 = Cons 'c' (Cons 'a' (Cons 't' Empty)) :: List Char

-- E.g. map' (*2) list1 == Cons 2 (Cons 4 (Cons 6 Empty))
map' :: (a -> b) -> List a -> List b
map' _ Empty       = Empty
map' f (Cons x xs) = Cons (f x) (map' f xs)

--
--
-- Types with more than one parameter
--
--
-- E.g. tuple
-- i.e. data (,) a b = (,) a b
--
itemCount1 = ("Erasers", 25) :: (String, Int)

itemCount2 = ("Pencils", 25) :: (String, Int)

itemCount3 = ("Pens", 13) :: (String, Int)

itemInventory = [itemCount1, itemCount2, itemCount3] :: [(String, Int)]

--
-- QC1803
-- Error, because 12.4 is not an Int.
--
--
-- Kinds
--
-- QC1804
-- :k (,,) ==> (,,) :: * -> * -> * -> *
--
--
-- Map
--
data Organ
  = Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Show, Eq, Ord)

organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney] :: [Organ]

ids = [2, 7, 13, 14, 21, 24] :: [Int]

-- pairs == [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]
pairs = zip ids organs :: [(Int, Organ)]

-- catalog ==
--   fromList [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]
catalog = Map.fromList pairs :: Map.Map Int Organ

-- Map.lookup 7 catalog == Just Heart
--
--
-- Q1801
--
-- E.g. tripleMap (+1) aPoint == Triple 1.1 54.2 13.3
tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

-- E.g. boxMap (*2) (Box 4) == Box 8
boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)
--
--
-- Q1802
pairs' = zip organs ids :: [(Organ,Int)]
catalog' = Map.fromList pairs' :: Map.Map Organ Int
-- WRONG
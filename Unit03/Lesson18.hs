module Lesson18 where

import qualified Data.Map                      as Map

-- Consider this

type Latitude = Double
type Longitude = Double

data Coordinate = Coordinate Latitude Longitude deriving (Show)

ct1 = Coordinate 40.632527 (-74.020869)

--
-- Types that take arguments
--

-- Simplest parameterized type:

newtype Box a = Box a deriving (Show)

wrap :: a -> Box a
wrap = Box

unwrap :: Box a -> a
unwrap (Box x) = x

-- QC1
-- :t wrap (Box 'a') --> Box (Box Char)

-- A more useful parameterized type

data Triple a = Triple a a a deriving (Show)

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

-- Accessors

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

--

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

{-
transform (* 3) aPoint -- Triple 0.30000000000000004 159.60000000000002 36.900000000000006
transform reverse aPerson -- Triple "drawoH" "spillihP" "tfarcevoL"
-}
transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- QC2
-- `map` can return a list with a different type from the original; `transform` can't.

-- Lists

-- Implement my own List type
-- i.e. data [] a = [] | a:[a]

data List a = Empty | Cons a (List a) deriving (Eq, Show)

list1 :: List Int
list1 = Cons 1 (Cons 2 (Cons 3 Empty))

list2 :: List Char
list2 = Cons 'c' (Cons 'a' (Cons 't' Empty))

{-
map' (*2) list1 -- Cons 2 (Cons 4 (Cons 6 Empty))
-}
map' :: (a -> b) -> List a -> List b
map' _ Empty       = Empty
map' f (Cons x xs) = Cons (f x) (map' f xs)

--
-- Types with more than one parameter
--

-- E.g. tuple
-- i.e. data (,) a b = (,) a b

itemCount1 :: (String, Int)
itemCount1 = ("Erasers", 25)

itemCount2 :: (String, Int)
itemCount2 = ("Pencils", 25)

itemCount3 :: (String, Int)
itemCount3 = ("Pens", 13)

itemInventory :: [(String, Int)]
itemInventory = [itemCount1, itemCount2, itemCount3]

-- QC3
-- Error, because 12.4 is not an Int.

-- Kinds

-- QC4
-- :k (,,) ==> (,,) :: * -> * -> * -> *

-- Data.Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum, Bounded)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

pairs :: [(Int, Organ)]
pairs = zip ids organs
  -- [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]

catalog :: Map.Map Int Organ
catalog = Map.fromList pairs
  -- fromList [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]

{-
Map.lookup 7 catalog -- Just Heart
-}

-- Q1

{-
tripleMap (+1) aPoint -- Triple 1.1 54.2 13.3
-}
tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

{-
boxMap (*2) (Box 4) -- Box 8
-}
boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

-- Q2
-- Cheat

values :: [Organ]
values = map snd (Map.toList catalog) -- [Heart,Heart,Brain,Spleen,Spleen,Kidney]

allOrgans :: [Organ]
allOrgans = [minBound .. maxBound] -- [Heart,Brain,Kidney,Spleen]

-- Walk through the list of possible organs and count the number of
-- matches of each in our catalog
organCounts :: [Int]
organCounts = map countOrgan allOrgans
  where countOrgan organ = (length . filter (== organ)) values -- [2,1,1,2]

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans organCounts)
  -- fromList [(Heart,2),(Brain,1),(Kidney,1),(Spleen,2)]

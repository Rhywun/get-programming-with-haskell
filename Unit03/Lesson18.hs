module Lesson18 where

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

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)
-- Hm. This looks like a functor?

-- Cont. p. 205

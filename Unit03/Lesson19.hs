module Lesson19 where

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe

data Organ
  = Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

--
-- Map.lookup 7 organCatalog == Just Heart
-- Map.lookup 6 organCatalog == Nothing
--
--
-- QC1901
-- Map.lookup 6 organCatalog :: Maybe Organ
--
--
-- The problem with null
--
--
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map (`Map.lookup` catalog) ids

-- availableOrgans == [Nothing,Just Heart,Nothing,...etc...]
availableOrgans =
  getDrawerContents possibleDrawers organCatalog :: [Maybe Organ]

-- countOrgan Heart availableOrgans == 2
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter (\x -> x == Just organ) available)

--
--
-- Computing with Maybe
--
--
--
-- justTheOrgans == [Just Heart,Just Heart,Just Brain,Just Spleen,Just Spleen,Just Kidney]
justTheOrgans = filter isJust availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing      = ""

-- organList == ["Heart","Heart","Brain","Spleen","Spleen","Kidney"]
organList = map showOrgan justTheOrgans :: [String]

-- cleanList == "Heart, Heart, Brain, Spleen, Spleen, Kidney"
cleanList = intercalate ", " organList :: String

--
-- QC1902
numOrZero :: Maybe Int -> Int
numOrZero Nothing  = 0
numOrZero (Just n) = n

--
--
-- Q1901
-- emptyDrawers == 44
emptyDrawers = length (filter (== Nothing) availableOrgans) :: Int

--
-- Q1902
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing  = Nothing
maybeMap f (Just a) = Just (f a)

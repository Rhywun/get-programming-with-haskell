module Lesson19 where

import           Data.List                      ( intercalate )
import qualified Data.Map                      as Map
import           Data.Maybe

-- Consider this

groceries :: Map.Map String Int
groceries = Map.fromList [("Milk", 1), ("Candy bars", 10), ("Cheese blocks", 2)]

ct1 = Map.lookup "Milk" groceries -- Just 1
ct2 = Map.lookup "MILK" groceries -- Nothing

--
-- Introducing Maybe: solving missing values with types
--

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

{-
Map.lookup 7 organCatalog -- Just Heart
Map.lookup 6 organCatalog -- Nothing
-}

-- QC1
-- Map.lookup 6 organCatalog :: Maybe Organ

--
-- The problem with null
--

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

{-
getDrawerContents [12, 13] organCatalog -- [Nothing, Just Brain]
-}
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map (`Map.lookup` catalog) ids

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog
  -- [Nothing,Just Heart,Nothing,Nothing,Nothing,Nothing,Just Heart,Nothing,..etc..]

{-
countOrgan Heart availableOrgans -- 2
-}
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter (\x -> x == Just organ) available)

--
-- Computing with Maybe
--

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isJust availableOrgans
  -- [Just Heart,Just Heart,Just Brain,Just Spleen,Just Spleen,Just Kidney]

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing      = ""

organList :: [String]
organList = map showOrgan justTheOrgans
  -- ["Heart","Heart","Brain","Spleen","Spleen","Kidney"]

cleanList :: String
cleanList = intercalate ", " organList -- "Heart, Heart, Brain, Spleen, Spleen, Kidney"

-- QC2

{-
numOrZero Nothing  -- 0
numOrZero (Just 3) -- 3
-}
numOrZero :: Maybe Int -> Int
numOrZero Nothing  = 0
numOrZero (Just n) = n

--
-- Back to the lab! More-complex computation with Maybe
--

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ)    = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ)    = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving (Show)

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat    a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag    a) = (Kitchen, Bag a)

{-
process Brain  -- (Lab,Brain in a vat)
process Heart  -- (Lab,Heart in a cooler)
process Spleen -- (Kitchen,Spleen in a bag)
-}
process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

{-
report $ process Brain -- "Brain in a vat in the Lab"
-}
report :: (Location, Container) -> String
report (location, container) = show container ++ " in the " ++ show location

processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing      = "error, id not found"

{-
processRequest 13 organCatalog -- "Brain in a vat in the Lab"
processRequest 12 organCatalog -- "error, id not found"
-}
processRequest :: Int -> Map.Map Int Organ -> String
processRequest id' catalog = processAndReport organ where organ = Map.lookup id' catalog

-- QC3

report' :: Maybe (Location, Container) -> String
report' Nothing                      = "container not found"
report' (Just (location, container)) = show container ++ " in the " ++ show location

-- Q1

emptyDrawers :: Int
emptyDrawers = length (filter (== Nothing) availableOrgans) :: Int -- 44

-- Q2

{-
maybeMap (+ 1) (Just 2) -- Just 3
maybeMap (+ 1) Nothing  -- Nothing
-}
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing  = Nothing
maybeMap f (Just a) = Just (f a)

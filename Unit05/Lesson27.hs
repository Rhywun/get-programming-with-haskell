module Lesson27 where

import qualified Data.Map   as Map
import           Data.Maybe

--
--
-- An example: computing in a Maybe
--
--
successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing  = Nothing

-- QC2701
reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just x) = Just (reverse x)
reverseMaybe Nothing  = Nothing

--
--
-- Using functions in context with the Functor type class
--
--
{-
instance Functor Maybe where
  fmap func (Just n) = Just (func n)
  fmap func Nothing = Nothing
-}
--
-- Now we can do this:
--
e1 = (+ 1) <$> successfulRequest -- == Just 7

e2 = (+ 1) <$> failedRequest -- == Nothing

--
-- Maybe Int -> Maybe String
--
successStr :: Maybe String
successStr = show <$> successfulRequest -- == Just "6"

failStr :: Maybe String
failStr = show <$> failedRequest -- == Nothing

--
-- QC2702
e3 = reverse <$> Just "hello" -- == Just "olleh"

--
--
-- Functors are everywhere!
--
--
data RobotPart = RobotPart
  { name        :: String
  , description :: String
  , cost        :: Double
  , count       :: Int
  } deriving (Show)

leftArm :: RobotPart
leftArm =
  RobotPart
  { name = "left arm"
  , description = "left arm for face punching!"
  , cost = 1000.00
  , count = 3
  }

rightArm :: RobotPart
rightArm =
  RobotPart
  { name = "right arm"
  , description = "right arm for kind hand gestures"
  , cost = 1025.00
  , count = 5
  }

robotHead :: RobotPart
robotHead =
  RobotPart
  { name = "robot head"
  , description = "this head looks mad"
  , cost = 5092.25
  , count = 2
  }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part =
  mconcat
    [ "<h2>"
    , partName
    , "</h2>"
    , "<p><h3>desc</h3>"
    , partDesc
    , "</p><p><h3>cost</h3>"
    , partCost
    , "</p><p><h3>count</h3>"
    , partCount
    , "</p>"
    ]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

-- Convert a Maybe RobotPart to Maybe Html
--
-- == Just (RobotPart {name = "left arm", description = ...})
partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partVal' = Map.lookup 999 partsDB -- == Nothing

-- == Just "<h2>left arm</h2><p><h3>desc</h3>left arm for face ..."
partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

-- Convert a list of RobotPart to a list of Html
--
allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts -- same as `map renderHtml allParts`

-- QC2703
allParts' = snd <$> Map.toList partsDB

-- Convert a Map of RobotPart to a Map of Html
--   --> Notice that only the 2nd type variable participates in the Functor!
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

-- Convert an IO RobotPart to an IO Html
--
leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

--
-- Q2701
newtype Box a =
  Box a
  deriving (Show)

-- E.g. (+1) <$> Box 2 == Box 3
instance Functor Box where
  fmap f (Box a) = Box (f a)

-- E.g. morePresents 5 (Box "toy") == Box ["toy","toy","toy","toy","toy"]
morePresents :: Int -> Box a -> Box [a]
morePresents n box = replicate n <$> box

--
-- Q2702
myBox :: Box Int
myBox = Box 1

wrapped = Box <$> myBox -- == Box (Box 1)

unwrap :: Box a -> a
unwrap (Box x) = x

unwrapped = unwrap <$> wrapped -- == Box 1

--
-- Q2703
main :: IO ()
main = do
  putStr "ID? "
  input <- getLine
  let part = Map.lookup (read input) partsDB
  if isJust part
    then do
      putStr "Cost: "
      print (fromJust (cost <$> part))
    else putStrLn "Not found."

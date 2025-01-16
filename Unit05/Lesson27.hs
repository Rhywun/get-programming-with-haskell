{-# LANGUAGE InstanceSigs #-}

module Lesson27 where

import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)

-- Consider this:

printInt :: Maybe String -> IO ()
printInt Nothing = putStrLn "value missing"
printInt (Just val) = putStrLn val

-- How do we convert a Maybe Int to Maybe String to pass into this function?

intToStr :: Int -> String
intToStr x = show (x * x) ++ "!"

-- The answer will be, use `Functor` (i.e. `fmap`)

-- * An example: computing in a Maybe

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

-- >>> incMaybe successfulRequest
-- >>> incMaybe failedRequest
-- Just 7
-- Nothing
incMaybe :: Maybe Int -> Maybe Int
incMaybe Nothing = Nothing
incMaybe (Just n) = Just (n + 1)

-- QC1

-- >>> reverseMaybe $ Just "hello"
-- >>> reverseMaybe Nothing
-- Just "olleh"
-- Nothing
reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just x) = Just (reverse x)

-- * Using functions in context with the Functor type class

{-
fmap :: Functor g => (a -> b) -> g a -> g b
-}

{-
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap func (Just n) = Just (func n)
  fmap func Nothing = Nothing
-}

-- Now we can do this, without having to create a wrapper function:

e1 :: Maybe Int
e1 = (+ 1) <$> successfulRequest -- Just 7

e2 :: Maybe Int
e2 = (+ 1) <$> failedRequest -- Nothing

-- Maybe Int -> Maybe String

successStr :: Maybe String
successStr = show <$> successfulRequest -- Just "6"

failStr :: Maybe String
failStr = show <$> failedRequest -- Nothing

-- QC2

qc2 :: Maybe String
qc2 = reverse <$> Just "hello" -- Just "olleh"

-- ! An aside: Note that the name of <$> is modeled after $ -
-- ! thus, you don't need parentheses around `Just "hello"`

-- * Functors are everywhere!

data RobotPart = RobotPart
  { name :: String,
    description :: String,
    cost :: Double,
    count :: Int
  }
  deriving (Show)

leftArm :: RobotPart
leftArm =
  RobotPart
    { name = "left arm",
      description = "left arm for face punching!",
      cost = 1000.00,
      count = 3
    }

rightArm :: RobotPart
rightArm =
  RobotPart
    { name = "right arm",
      description = "right arm for kind hand gestures",
      cost = 1025.00,
      count = 5
    }

robotHead :: RobotPart
robotHead =
  RobotPart
    { name = "robot head",
      description = "this head looks mad",
      cost = 5092.25,
      count = 2
    }

type Html = String

-- | Rendering a RobotPart as HTML
renderHtml :: RobotPart -> Html
renderHtml part =
  mconcat
    [ "<h2>",
      partName,
      "</h2>",
      "<p><h3>desc</h3>",
      partDesc,
      "</p><p><h3>cost</h3>",
      partCost,
      "</p><p><h3>count</h3>",
      partCount,
      "</p>"
    ]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)

-- | RobotPart "database"
partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

-- * Converting a Maybe RobotPart to Maybe Html

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB
-- ^ Just (RobotPart {name = "left arm", description = ...})

partVal' :: Maybe RobotPart
partVal' = Map.lookup 999 partsDB
-- ^ Nothing

-- >>> renderHtml <$> partVal
-- Just "<h2>left arm</h2><p><h3>desc</h3>left arm for face punching!</p><p><h3>cost</h3>1000.0</p><p><h3>count</h3>3</p>"
partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

-- * Converting a list of RobotParts to a list of Html

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts -- same as `map renderHtml allParts`

-- QC3

allParts' :: [RobotPart]
allParts' = snd <$> Map.toList partsDB

-- * Converting a Map of RobotParts to a Map of HTML

-- NOTE - Notice that only the 2nd type variable (the value) participates in the Functor!

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

-- * Converting an IO RobotPart to an IO Html

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

-- Q1

newtype Box a = Box a deriving (Show)

-- >>> (+ 1) <$> Box 2
-- Box 3
instance Functor Box where
  fmap :: (a -> b) -> Box a -> Box b
  fmap f (Box a) = Box (f a)

-- >>> morePresents 5 (Box "toy")
-- Box ["toy","toy","toy","toy","toy"]
morePresents :: Int -> Box a -> Box [a]
morePresents n box = replicate n <$> box

-- Q2

myBox :: Box Int
myBox = Box 1

wrapped :: Box (Box Int)
wrapped = Box <$> myBox -- Box (Box 1)

unwrap :: Box a -> a
unwrap (Box x) = x

unwrapped :: Box Int
unwrapped = unwrap <$> wrapped -- Box 1

-- Q3

-- The answer in the book is very similar but avoids `fromJust` and the `if`
-- statement.
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

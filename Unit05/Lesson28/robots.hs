module Robots where

import qualified Data.Map                      as Map

data RobotPart = RobotPart
  { name        :: String
  , description :: String
  , cost        :: Double
  , count       :: Int
  } deriving (Show)

leftArm = RobotPart "left arm" "left arm for face punching!" 1000.0 3

rightArm = RobotPart "right arm" "right arm for kind hand gestures" 1025.0 5

robotHead = RobotPart "robot head" "this head looks mad" 5092.25 2

leftLeg = RobotPart "left leg" "left leg for kicking!" 1225.5 3

rightLeg = RobotPart "right leg" "right leg for dancing" 1119.99 2

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
 where
  keys    = [1, 2, 3, 4, 5]
  vals    = [leftArm, rightArm, robotHead, leftLeg, rightLeg]
  keyVals = zip keys vals

getLowerCost :: Maybe RobotPart -> Maybe RobotPart -> Maybe Double
getLowerCost p1 p2 = min <$> (cost <$> p1) <*> (cost <$> p2)

{-
     (RobotPart -> Double)
           vvvv
           cost <$> p1           -> Maybe Double
                    ^^
              Maybe RobotPart
-}

printLowerCost :: Maybe Double -> IO ()
printLowerCost Nothing     = putStrLn "At least one part not found."
printLowerCost (Just cost) = putStrLn (show cost ++ " is lower cost.")

main :: IO ()
main = do
  putStr "ID 1? "
  input1 <- getLine
  let part1 = Map.lookup (read input1) partsDB
  putStr "ID 2? "
  input2 <- getLine
  let part2 = Map.lookup (read input2) partsDB
  let lower = getLowerCost part1 part2
  printLowerCost lower

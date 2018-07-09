import qualified Data.Map                      as Map

type Size = Double

area :: Size -> Double
area size = pi * (size / 2) ^ (2 :: Int)

type Cost = Double

type Pizza = (Size, Cost)

costPerSqIn :: Pizza -> Double
costPerSqIn (size, cost) = cost / area size

cheaperPizza :: Pizza -> Pizza -> Pizza
cheaperPizza p1 p2 = case compare (costPerSqIn p1) (costPerSqIn p2) of
  LT -> p1
  _  -> p2

describePizza :: Pizza -> String
describePizza (size, cost) =
  "The " ++ show size ++ "\" pizza is cheaper at " ++ show cpsi ++ " per sq. in."
  where cpsi = costPerSqIn (size, cost)

main :: IO ()
main = do
  putStr "What is the size of pizza 1? "
  size1 <- getLine
  putStr "What is the cost of pizza 1? "
  cost1 <- getLine
  putStr "What is the size of pizza 2? "
  size2 <- getLine
  putStr "What is the cost of pizza 2? "
  cost2 <- getLine
  let pizza1      = (read size1, read cost1)
  let pizza2      = (read size2, read cost2)
  let betterPizza = cheaperPizza pizza1 pizza2 -- cheaper is better!
  putStrLn (describePizza betterPizza)

--
-- A peek at Monad - do-notation in Maybe
--
costData :: Map.Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  let pizza1      = (size1, cost1)
  let pizza2      = (size2, cost2)
  let betterPizza = cheaperPizza pizza1 pizza2
  return (describePizza betterPizza)

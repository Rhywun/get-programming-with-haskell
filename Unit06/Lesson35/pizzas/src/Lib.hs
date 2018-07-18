module Lib
  ( cheaperPizza
  , describePizza
  )
where

type Size = Double

area :: Size -> Double
area size = pi * (size / 2) ^ 2

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

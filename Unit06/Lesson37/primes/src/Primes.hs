module Primes where

primes :: [Int]
primes = sieve [2 .. 10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where
    noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

isPrime :: Int -> Maybe Bool
isPrime n
  | n < 0 = Nothing
  | n >= length primes = Nothing
  | otherwise = Just (n `elem` primes)

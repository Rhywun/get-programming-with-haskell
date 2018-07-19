module Primes where

-- List the primes in the given range
{-
sieve [2..20] -- [2,3,5,7,11,13,17,19]
-}
sieve :: [Int] -> [Int]
sieve []                 = []
sieve (nextPrime : rest) = nextPrime : sieve noFactors
  where noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

-- A list of primes of "reasonable" length. (Note that an upper limit of, say,
-- 100,000 will still take a very long time to execute upon first usage.)
{-
length primes  -- 1229
take 10 primes -- [2,3,5,7,11,13,17,19,23,29]
-}
primes :: [Int]
primes = sieve [2 .. 10000]

-- Is it prime?
{-
isPrime 8    -- Just False
isPrime 17   -- Just True
isPrime (-1) -- Nothing
-}
isPrime :: Int -> Maybe Bool
isPrime n | n < 2              = Nothing
          | n >= length primes = Nothing
          | otherwise          = Just (n `elem` primes)


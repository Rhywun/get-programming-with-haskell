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

{-
unsafePrimeFactors 20 primes -- [2,2,5]
-}
unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 []              = []
unsafePrimeFactors n []              = []
unsafePrimeFactors n (next : primes) = if n `mod` next == 0
  then next : unsafePrimeFactors (n `div` next) (next : primes)
  else unsafePrimeFactors n primes

{-
primeFactors 20 -- Just [2,2,5]
-}
primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2              = Nothing
               | n >= length primes = Nothing
               | otherwise          = Just (unsafePrimeFactors n primesLessThanN)
  where primesLessThanN = filter (<= n) primes

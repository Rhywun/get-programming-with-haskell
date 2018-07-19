import           Data.Maybe
import           Test.QuickCheck
import           Primes

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheck prop_primesArePrime
  quickCheck prop_nonPrimesAreComposite
  putStrLn "Done."

-- Values outside the given range should return Nothing, inside Just
prop_validPrimesOnly n = if n < 2 || n >= length primes
  then isNothing result
  else isJust result
  where result = isPrime n

prop_primesArePrime n = if result == Just True then length divisors == 0 else True
 where
  result   = isPrime n
  divisors = filter ((== 0) . (n `mod`)) [2 .. (n - 1)]

prop_nonPrimesAreComposite n = if result == Just False then length divisors > 0 else True
 where
  result   = isPrime n
  divisors = filter ((== 0) . (n `mod`)) [2 .. (n - 1)]

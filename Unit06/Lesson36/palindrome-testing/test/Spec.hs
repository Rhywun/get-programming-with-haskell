import           Lib
import           Data.Char                      ( isPunctuation )
import           Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Text as T

-- assert :: Bool -> String -> String -> IO ()
-- assert test pass fail = if test then putStrLn pass else putStrLn fail

prop_punctuationInvariant :: T.Text -> Bool
prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant :: T.Text -> Bool
prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)

main :: IO ()
main = do
  putStrLn "Running tests..."

  -- assert (isPalindrome "racecar")     "passed 'racecar'"  "FAIL: 'racecar'"
  -- assert (isPalindrome "racecar!")    "passed 'racecar!'" "FAIL: 'racecar!'"
  -- assert ((not . isPalindrome) "cat") "passed 'cat'"      "FAIL: 'cat'"
  -- assert (isPalindrome "racecar.")    "passed 'racecar.'" "FAIL: 'racecar.'"

  -- quickCheck prop_punctuationInvariant
  -- quickCheckWith stdArgs { chatty = True } prop_punctuationInvariant
  -- verboseCheck prop_punctuationInvariant

  quickCheck prop_punctuationInvariant
  quickCheck prop_reverseInvariant

  -- putStrLn "done!"

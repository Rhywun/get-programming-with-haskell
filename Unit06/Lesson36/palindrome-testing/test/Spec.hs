import           Lib
import           Data.Char                      ( isPunctuation
                                                , isSpace
                                                , toLower
                                                )
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import           Data.Text                     as T

-- assert :: Bool -> String -> String -> IO ()
-- assert test pass fail = if test then putStrLn pass else putStrLn fail

prop_punctuationInvariant :: T.Text -> Bool
prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text

prop_whitespaceInvariant :: T.Text -> Bool
prop_whitespaceInvariant text = preprocess text == preprocess noWhitespaceText
  where noWhitespaceText = T.filter (not . isSpace) text

prop_reverseInvariant :: T.Text -> Bool
prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)

main :: IO ()
main = do
  putStrLn "Running tests..."
  quickCheck prop_punctuationInvariant
  quickCheck prop_whitespaceInvariant
  quickCheck prop_reverseInvariant
  putStrLn "done!"

import           Data.Char       (isPunctuation, isSpace, toLower)
import           Lib
import           Test.QuickCheck

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where
    noPuncText = filter (not . isPunctuation) text

prop_whitespaceInvariant text = preprocess text == preprocess noWhitespaceText
  where
    noWhitespaceText = filter (not . isSpace) text

prop_caseInvariant text = preprocess text == preprocess (map toLower text)

prop_reverseInvariant text = isPalindrome text == isPalindrome (reverse text)

main :: IO ()
main = do
  quickCheck prop_punctuationInvariant
  quickCheck prop_whitespaceInvariant
  quickCheck prop_caseInvariant
  quickCheck prop_reverseInvariant
  putStrLn "Done."

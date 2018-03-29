module Palindrome
  ( isPalindrome
  ) where

import           Data.Char (isPunctuation, isSpace)
import           Data.Text as T (Text, filter, toLower, reverse)

stripWhiteSpace :: Text -> Text
stripWhiteSpace = T.filter (not . isSpace)

stripPunctuation :: Text -> Text
stripPunctuation = T.filter (not . isPunctuation)

toLowerCase :: Text -> Text
toLowerCase = T.toLower

preprocess :: Text -> Text
preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where
    cleanText = preprocess text

module Lib
  ( isPalindrome
  , preprocess
  )
where

import           Data.Text                     as T
import           Data.Char                      ( toLower
                                                , isSpace
                                                , isPunctuation
                                                )

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . isSpace)

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText where cleanText = preprocess text

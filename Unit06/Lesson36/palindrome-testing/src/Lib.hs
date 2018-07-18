module Lib
  ( isPalindrome
  , preprocess
  )
where

import Data.Text as T
import Data.Char (isPunctuation)

preprocess :: T.Text -> T.Text
preprocess = T.filter (not . isPunctuation)

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText where cleanText = preprocess text

{-# LANGUAGE OverloadedStrings #-}

module Lesson23 where

import           Data.Semigroup                 ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TLIO

--
-- Using Data.Text
--

word1 :: String
word1 = "pessimism"

word2 :: T.Text
word2 = T.pack word1

word3 :: String
word3 = T.unpack word2

-- QC1

word4 :: T.Text
word4 = T.pack word3

-- With OverloadedStrings language extension:

sampleInput :: T.Text
sampleInput = "this\nis some\ninput"

{-
T.lines sampleInput -- ["this","is some","input"]
T.words sampleInput -- ["this","is","some","input"]
T.splitOn (T.pack "is") sampleInput -- ["th","\n"," some\ninput"]
T.unlines (T.lines sampleInput) -- "this\nis some\ninput\n"
T.unwords (T.words sampleInput) -- "this is some input"
T.intercalate (T.pack ",") (T.words sampleInput) -- "this,is,some,input"
-}

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"] -- "some text"

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text" -- "some text"

-- QC3

lines' :: T.Text -> [T.Text]
lines' = T.splitOn "\n"

unlines' :: [T.Text] -> T.Text
unlines' = T.intercalate "\n"

--
-- Text and Unicode & Text I/O
-- see bg_highlight.hs
--

-- Q1
-- See hello_world.hs

-- Q2
-- Cheated.
-- Q: Why is there no lazy read?

toInts :: TL.Text -> [Int]
toInts = map (read . TL.unpack) . TL.lines

main2 :: IO ()
main2 = do
  input <- TLIO.getContents
  let numbers = toInts input
  TLIO.putStrLn (TL.pack (show (sum numbers)))

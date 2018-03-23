{-# LANGUAGE OverloadedStrings #-}

module Lesson23 where

import           Data.Semigroup
import qualified Data.Text      as T

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

--
-- QC2301
fourthWord :: T.Text
fourthWord = T.pack thirdWord

--
-- With language extension:
--
sampleInput :: T.Text
sampleInput = "this\nis some\ninput"

--
--
-- NOTE: None of these "work" because of the Text type
--
-- T.lines sampleInput == ["this","is some","input"]
-- T.words sampleInput == ["this","is","some","input"]
-- T.splitOn (T.pack "is") sampleInput == ["th","\n"," some\ninput"]
-- T.unlines (T.lines sampleInput) == "this\nis some\ninput\n"
-- T.unwords (T.words sampleInput) == "this is some input"
-- T.intercalate (T.pack ",") (T.words sampleInput) == "this,is,some,input"
--
--
combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

--
--
-- QC2303
lines' :: T.Text -> [T.Text]
lines' = T.splitOn "\n"

unlines' :: [T.Text] -> T.Text
unlines' = T.intercalate "\n"

--
--
-- Q2301
-- See hello_world.hs

--
-- cont. p. 281

{-# LANGUAGE OverloadedStrings #-}

module Lesson23 where

import           Data.Semigroup
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO

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
-- Q2302
-- Cheated.
-- Q: Why is there no lazy read?
--
toInts :: TL.Text -> [Int]
toInts = map (read . TL.unpack) . TL.lines

main2 :: IO ()
main2 = do
  input <- TLIO.getContents
  let numbers = toInts input
  TLIO.putStrLn (TL.pack (show (sum numbers)))

{-# LANGUAGE OverloadedStrings #-}

module Lesson25 where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Text.IO                  as TIO


-- Consider this:

tatsuhikoTakimoto :: T.Text
tatsuhikoTakimoto = "滝本 竜彦"

-- How many bytes? This is not the correct answer:
{-
T.length tatsuhikoTakimoto -- 5
-}

-- Answer:
-- ???

--
-- Working with binary data by using ByteString
--

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

sampleString :: String
-- sampleString = B.unpack sampleBytes      <-- this doesn't work
sampleString = BC.unpack sampleBytes

-- QC1

bcInt :: BC.ByteString
bcInt = "6"

{-
bcbs2int bcInt -- 6
-}
bcbs2int :: BC.ByteString -> Int
bcbs2int x = read $ BC.unpack x

--
-- Glitching JPEGs
-- see glitcher.hs
--

--
-- ByteStrings, Char8, and Unicode
--

nagarjunaBC :: BC.ByteString
nagarjunaBC = "नागर्जुनॅ" -- "(>\ETB0M\FSA(E"

nagarjunaText :: T.Text
nagarjunaText = "नागर्जुनॅ" -- "\2344\2366\2327\2352\2381\2332\2369\2344\2373"

nagarjunaB :: B.ByteString
nagarjunaB = (BC.pack . T.unpack) nagarjunaText -- "(>\ETB0M\FSA(E"

-- You'll need Data.Text.Encoding to make this work!
-- (see text)

-- Q1
-- cheat

q1 :: IO ()
q1 = do
  input <- B.readFile "tatsuhiko.txt"
  putStr "Bytes: "
  print (B.length input)
  putStr "Chars: "
  print ((T.length . E.decodeUtf8) input)

-- Q2
-- pass

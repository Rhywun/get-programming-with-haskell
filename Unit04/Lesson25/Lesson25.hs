{-# LANGUAGE OverloadedStrings #-}

module Lesson25 where

import qualified Data.Text                     as T
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC

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
nagarjunaBC = "नागर्जुनॅ"

nagarjunaText :: T.Text
nagarjunaText = "नागर्जुनॅ"

-- skip the rest yawn

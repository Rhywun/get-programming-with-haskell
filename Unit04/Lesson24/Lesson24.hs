module Lesson24 where

import           System.IO

--
-- Opening and closing files
--

-- QC1
-- openFile "stuff.txt" ReadMode

main1 :: IO ()
main1 = do
  file <- openFile "hello.txt" ReadMode
  hClose file
  putStrLn "Done."

main2 :: IO ()
main2 = do
  inputFile <- openFile "hello.txt" ReadMode
  line1     <- hGetLine inputFile
  putStrLn line1
  line2      <- hGetLine inputFile
  outputFile <- openFile "goodbye.txt" AppendMode
  hPutStrLn outputFile line2
  hClose inputFile
  hClose outputFile
  putStrLn "Done."

main3 :: IO ()
main3 = do
  helloFile <- openFile "hello1.txt" ReadMode
  eof       <- hIsEOF helloFile
  firstLine <- if not eof then hGetLine helloFile else return "empty"
  putStrLn firstLine
  -- QC2
  eof        <- hIsEOF helloFile
  secondLine <- if not eof then hGetLine helloFile else return "no second line"
  putStrLn secondLine
  putStrLn "done!"

--
-- Simple I/O tools
-- See fileCounts.hs
--

--
-- The trouble with lazy I/O
-- see fileCounts.hs, 2nd version of `main`
--

-- QC4
-- Because it's lazy - the data can be read any time later in the program.

--
-- Strict I/O
-- See fileCount_strict.hs
--

-- Q1
-- See cp.hs

-- Q2
-- See capitalize.hs

-- FIXED - I'm still not clear on the difference between let= and <-
--   ==> Use <- to assign a value of type IO a and let it behave like type a
--       Use let= to assign a value that isn't an IO type

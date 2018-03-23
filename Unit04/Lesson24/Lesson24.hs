module Lesson24 where

import           System.IO

--
--
-- Opening and closing files
--
--
--
-- QC2401
-- openFile "stuff.txt" ReadMode
main1 :: IO ()
main1 = do
  file <- openFile "hello.txt" ReadMode
  hClose file
  putStrLn "Done."

main2 :: IO ()
main2 = do
  inputFile <- openFile "hello.txt" ReadMode
  line1 <- hGetLine inputFile
  putStrLn line1
  line2 <- hGetLine inputFile
  outputFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn outputFile line2
  hClose inputFile
  hClose outputFile
  putStrLn "Done."

main3 :: IO ()
main3 = do
  helloFile <- openFile "hello.txt" ReadMode
  eof <- hIsEOF helloFile
  firstLine <-
    if not eof
      then hGetLine helloFile
      else return "empty"
  -- QC2402
  eof <- hIsEOF helloFile
  secondLine <-
    if not eof
      then hGetLine helloFile
      else return "no second line"
  putStrLn "done!"

--
--
-- Simple I/O tools
--
-- See fileCounts.hs

--
-- QC2403
-- Because it's more convenient to input a list rather that apply ++ between each item.
-- Also, ++ is only defined for [Char], not Text.
--

--
--
--  The trouble with lazy I/O
--
-- See fileCounts.hs, 2nd version of `main`

--
-- QC2404
-- Because it's lazy - the data can be read any time later in the program.

--
--
-- Strict I/O
--
-- See fileCount_strict.hs
--

--
-- Q2401
-- See cp.hs



-- cont. p. 293
-- TODO - I'm still not clear on the difference between let= and <-
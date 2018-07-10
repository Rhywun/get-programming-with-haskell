import           System.Environment
import           System.IO

{-
getCounts "Hello, world!\nGood-bye, world!" -- (30, 4, 2)
-}
getCounts :: String -> (Int, Int, Int)
getCounts xs = (length xs, (length . words) xs, (length . lines) xs)

{-
describeCounts (30, 4, 2) -- "chars:  30  words:  4  lines:  2"
-}
describeCounts :: (Int, Int, Int) -> String
describeCounts (cc, wc, lc) =
  unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc]

-- QC3
-- It is preferable to use `unwords` because we might want to use Text.

-- This version won't work on stats.dat becasue the file is locked while writing
{-
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- readFile fileName
  let summary = (describeCounts . getCounts) input
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
  putStrLn summary
-}

-- This version should work on stats.dat, because we've closed the file after its
-- contents are read
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = (describeCounts . getCounts) input
  putStrLn summary
  hClose file -- moved here to prevent lazy evaluation error
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])

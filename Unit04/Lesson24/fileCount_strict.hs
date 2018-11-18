{-# LANGUAGE OverloadedStrings #-}

import           System.Environment
import           System.IO
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TI

{-
getCounts "Hello, world!\nGood-bye, world!" == (30, 4, 2)
-}
getCounts :: T.Text -> (Int, Int, Int)
getCounts xs = (T.length xs, (length . T.words) xs, (length . T.lines) xs)

{-
describeCounts (30, 4, 2) == "chars:  30  words:  4  lines:  2"
-}
describeCounts :: (Int, Int, Int) -> T.Text
describeCounts (cc, wc, lc) =
  T.pack (unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc])

-- This version solves the locking issue on stats.dat
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TI.readFile fileName
  let summary = (describeCounts . getCounts) input
  TI.appendFile "stats.dat" (mconcat [T.pack fileName, " ", summary, "\n"])
  TI.putStrLn summary

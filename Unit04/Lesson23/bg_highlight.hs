{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

dharma :: T.Text
dharma = "धर्म"

bgText :: T.Text
bgText =
  "श्रेयान्स्वधर्मोविगुणःपरधर्मात्स्वनुष्ठितात्।स्वधर्मेनिधनंश्रेयःपरधर्मो"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where
    pieces = T.splitOn query fullText
    highlighted = mconcat ["{", query, "}"]

main :: IO ()
main = TIO.putStrLn (highlight dharma bgText)

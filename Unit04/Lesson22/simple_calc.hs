import           Data.List.Split

{-
isPlus "1 + 2" -- True
-}
isPlus :: String -> Bool
isPlus = elem '+'

{-
isMult "1 + 2" -- False
-}
isMult :: String -> Bool
isMult = elem '*'

{-
splitEquation "1 + 2" -- (1, 2)
-}
splitEquation :: String -> (Int, Int)
splitEquation eq
  | isPlus eq = (read (head sp), read (last sp))
  | isMult eq = (read (head sm), read (last sm))
  where
    sp = splitOn "+" eq
    sm = splitOn "*" eq

{-
evalEquation "12 + 34" -- 46
evalEquation "56 * 78" -- 4368
-}
evalEquation :: String -> Int
evalEquation eq
  | isPlus eq = l + r
  | isMult eq = l * r
  where
    lr = splitEquation eq
    l = fst lr
    r = snd lr

main :: IO ()
main = do
  input <- getContents
  let results = map evalEquation (lines input)
  print results

--
-- POST MORTEM
-- My solution prints the results after all equations are input
--
-- Perhaps we were supposed to use the non-lazy technique here?
--

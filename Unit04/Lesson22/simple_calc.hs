import           Data.List.Split

-- E.g. isPlus "1 + 2" == True
isPlus :: String -> Bool
isPlus = elem '+'

-- E.g. isMult "1 + 2" == False
isMult :: String -> Bool
isMult = elem '*'

-- E.q. splitEquation "1 + 2" == (1, 2)
splitEquation :: String -> (Int, Int)
splitEquation eq
  | isPlus eq = (read (head sp), read (last sp))
  | isMult eq = (read (head sm), read (last sm))
  where
    sp = splitOn "+" eq
    sm = splitOn "*" eq

-- E.g. evalEquation "12 + 34" == 46
--      evalEquation "56 * 78" == 4368
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
-- This sort of works, but there's some garbage in the output that's displayed
-- to the screen.
-- E.g.:
{-
1+2
[34*7
,28]D
-}

module Lesson22 where

--
-- Interacting with the command line the nonlazy way
-- see sum.hs
--

-- QC1

main1 :: IO ()
main1 = do
  vals <- mapM (const getLine) [1 .. 3]
  mapM_ putStrLn vals

-- QC2

replicateM' :: (Monad m, Num a, Enum a) => a -> m b -> m [b]
replicateM' n f = mapM (const f) [1 .. n]

--
-- Interacting with lazy I/O
-- see sum_lazy.hs
--

-- QC3
-- see QC3.hs

-- QC4
-- see QC4.hs

-- Q1
-- see simple_calc.hs

-- Q2
-- see quotes.hs

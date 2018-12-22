module Lesson08 where

--
-- Consider this
--

{-
drop' 3 [1,2,3,4] -- 4
-}
drop' 0 xs       = xs
drop' n (x : xs) = drop' (n - 1) xs

--
-- Recursion on lists
--

-- length

{-
length' "hello" -- 5
-}
length' [] = 0
length' xs = 1 + length' (tail xs)

-- QC1

{-
length'' "hello" -- 5
-}
length'' []       = 0
length'' (x : xs) = 1 + length' xs

-- take

{-
take' 3 "hello" -- "hel"
take' 5 []      -- []
-}
take' _ []       = []
take' 0 _        = []
take' n (x : xs) = x : take' (n - 1) xs

-- cycle

{-
take' 10 (cycle' "heh") -- "hehhehhehh"
-}
cycle' (x : xs) = x : cycle' (xs ++ [x])

-- repeat

{-
take' 4 (repeat' "heh") -- ["heh","heh","heh","heh"]
-}
repeat' x = x : repeat' x

-- replicate

{-
replicate' 4 "heh" -- ["heh","heh","heh","heh"]
-}
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

--
-- Pathological recursion: Ackerman function and the Collatz conjecture
--

{-
:set +s
ackermann 3 3 -- 61, 0.00 secs
ackermann 3 8 -- 2045, 1.51 secs
ackermann 3 9 -- 4093, 6.08 secs        <-- Ouch, seriously slow!
ackermann 4 2 -- don't even bother - the answer has 19,729 digits!
:unset +s
-}
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

-- Counts the number of steps it takes the sequence to reach 1, starting at `n`
-- There is no known proof that this completes!
{-
collatz 9 -- 20
collatz 999 -- 50
collatz 92 -- 18
collatz 91 -- 93
map collatz [100..110] -- [26,26,26,88,13,39,13,101,114,114,114]
-}
collatz 1 = 1
collatz n = if even n then 1 + collatz (n `div` 2) else 1 + collatz (n * 3 + 1)

-- I prefer using guards here:

collatz' 1 = 1
collatz' n | even n    = 1 + collatz' (n `div` 2)
           | otherwise = 1 + collatz' (n * 3 + 1)

--
-- Summary
--

-- Q1

{-
reverse' "hello" -- "olleh"
-}
reverse' []       = []
reverse' (x : xs) = reverse' xs ++ [x]

-- Q2

-- This quickly grows out of control so let's replace it:
{-
fib 30 == 832040          <-- 1.56 sec
fib 35 == 9227465         <-- 17.36 sec
fib 40 == ?               <-- gave up
fib 1000 == ?             <-- forget about it
-}
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- The key is to remove one of the recursive calls:
fastFib n1 _  1       = n1
fastFib _  n2 2       = n2
fastFib n1 n2 3       = n1 + n2
fastFib n1 n2 counter = fastFib (n1 + n2) n1 (counter - 1)

{-
fib' 30 == 832040                  <-- 0.00 sec
fib' 35 == 9227465                 <-- 0.00 sec
fib' 1000 == ...long number...     <-- 0.01 sec
-}
fib' = fastFib 1 1

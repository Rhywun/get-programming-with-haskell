module Lesson08 where

--
-- Recursion on lists
--

length' []     = 0
length' (x:xs) = 1 + length' xs

take' _ []     = []
take' 0 _      = []
take' n (x:xs) = x : take' (n - 1) xs

cycle' (x:xs) = x : cycle' (xs ++ [x])

--
-- Pathological recursion: Ackerman function and the Collatz conjecture
--

-- E.g. ackermann 3 9 == 4093               <-- Ouch, seriously slow!
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

-- Counts the number of steps it takes the sequence to reach 1, starting at `n`
-- E.g. map collatz [100..120] ==
--        [26,26,26,88,13,39,13,101,114,114,114,70,21,13,34,34,21,21,34,34,21]
-- There is no known proof that this completes!
collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n * 3 + 1)

-- Q0801

reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Q0802

-- This quickly grows out of control so let's replace it
-- E.g. fib 30 == 832040          <-- 1.56 sec
--      fib 35 == 9227465         <-- 17.36 sec
--      fib 40 == ?               <-- gave up
--      fib 1000 == ?             <-- forget about it
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- The key is to remove one of the recursive calls
fastFib n1 _ 1  = n1
fastFib _ n2 2  = n2
fastFib n1 n2 3 = n1 + n2
fastFib n1 n2 counter = fastFib (n1 + n2) n1 (counter - 1)

-- E.g. fib 30 == 832040                  <-- 0.00 sec
--      fib 35 == 9227465                 <-- 0.00 sec
--      fib 1000 == ...long number...     <-- 0.00 sec
fib' = fastFib 1 1

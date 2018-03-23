fastFib :: Int -> Int -> Int -> Int
fastFib n1 _ 1  = n1
fastFib _ n2 2  = n2
fastFib n1 n2 3 = n1 + n2
fastFib n1 n2 counter = fastFib (n1 + n2) n1 (counter - 1)

-- E.g. fib 30 == 832040                  <-- 0.00 sec
--      fib 35 == 9227465                 <-- 0.00 sec
--      fib 1000 == ...long number...     <-- 0.00 sec
fib :: Int -> Int
fib = fastFib 1 1

main :: IO ()
main = do
  putStr "Number? "
  n <- getLine
  let result = fib $ read n
  putStrLn ("fib " ++ n ++ " = " ++ show result)

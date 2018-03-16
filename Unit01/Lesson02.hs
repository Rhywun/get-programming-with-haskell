simple x = x

-- QC0203

doublePlusTwo x = doubleX + 2
                  where doubleX = x * 2

-- Q0201
-- Because an if without an else would not return a value if the if branch eval'ed to F

-- Q0202

inc n = n + 1
double n = 2 * n
square n = n * n

-- Q0203

f n = if even n then n - 2 else 3 * n + 1

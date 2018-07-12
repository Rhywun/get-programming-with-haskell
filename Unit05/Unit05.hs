module Unit05 where

halve :: Int -> Double
halve n = fromIntegral n / 2.0

-- Given the tools we have so far, we need to write a wrapper in order to
-- work in a context:

halveMaybe :: Maybe Int -> Maybe Double
halveMaybe (Just n) = Just (halve n)
halveMaybe Nothing  = Nothing

-- And even that won't work with IO.

-- Thus, functors, applicatives, and monads.

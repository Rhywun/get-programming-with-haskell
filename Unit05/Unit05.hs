module Unit05 where

halve :: Int -> Double
halve n = fromIntegral n / 2.0

-- Given the tools we have so far, we need to write a wrapper in order to
-- work in a context:

-- >>> halveMaybe (Just 5)
-- Just 2.5
halveMaybe :: Maybe Int -> Maybe Double
halveMaybe (Just n) = Just (halve n)
halveMaybe Nothing = Nothing

-- But now we have to write a lot of wrappers.
-- And still there is no way to write a wrapper for IO.

-- Thus: functors, applicatives, and monads.

-- Sneak peek at functor:

-- >>> fmap halve (Just 5)
-- Just 2.5

module Lesson05 where

--
-- Closures - creating functions with functions
--

inc n = n + 1
double n = n * 2
square n = n ^ 2

ifEven f x = if even x then f x else x

genIfEven f = \x -> ifEven f x

ifEvenInc = genIfEven inc -- Partial application will simplify this later (`ifEven inc`)

-- QC1

genIfXEven x = \f -> ifEven f x

{-
genIf4Even inc -- 5
-}
genIf4Even = genIfXEven 4

{-
genIf5Even inc -- 5
-}
genIf5Even = genIfXEven 5 -- Any f you apply this to will not be called

--
-- Example: Generating URLs for an API
--

-- E.g. http://example.com/book/1234?token=1337hAsk3ll

getRequestURL host apiKey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host =
  (\apiKey resource id -> getRequestURL host apiKey resource id)

exampleUrlBuilder = genHostRequestBuilder "http://example.com"

genApiRequestBuilder hostBuilder apiKey =
  (\resource id -> hostBuilder apiKey resource id)

{-
myExampleUrlBuilder "book" "1234" -- "http://example.com/book/1234?token=1337hAsk3ll"
-}
myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

-- QC2

genApiRequestBuilder' hostBuilder resource apiKey =
  (\id -> hostBuilder apiKey resource id)

-- Partial application: making closures simple

add4 a b c d = a + b + c + d

{-
(addXto3 7) 1 2 3 -- 13
-}
addXto3 x = \b c d -> add4 x b c d

{-
mystery 2 3 4 -- 12
-}
mystery = add4 3 -- same as `addXto3 3`

-- Now we don't need "generator" functions any more:

exampleUrlBuilder' = getRequestURL "http://example.com"
myExampleUrlBuilder' = exampleUrlBuilder' "1337hAsk3ll"

-- QC3

{-
myBuilder "1234" -- "http://example.com/book/1234?token=1337hAsk3ll"
-}
myBuilder = getRequestURL "http://example.com" "1337hAsk3ll" "book"

--
-- Putting it all together
--

flipBinaryArgs f = \x y -> f y x -- same as `flip`

-- QC4

{-
subtract2 5 -- 3
-}
subtract2 = flip (-) 2

--
-- Summary
--

-- Q1

ifEvenInc' = ifEven inc
ifEvenDouble = ifEven double
ifEvenSquare = ifEven square

-- Q2

{-
(binaryPartialApplication (+) 2) 3 -- 5
-}
binaryPartialApplication f x = \y -> f x y

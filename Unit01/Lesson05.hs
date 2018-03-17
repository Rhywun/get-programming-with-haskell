module Lesson05 where

--
-- Closures - creating functions with functions
--

inc n = n + 1
double n = n * 2
square n = n ^ 2

ifEven f x = if even x then f x else x

genIfEven f = \x -> ifEven f x

ifEvenInc = genIfEven inc

-- QC0501

genIfXEven x = \f -> ifEven f x

genIf5Even = genIfXEven 5           -- Any f you apply this to will not be called

--
-- Example: Generating URLs for an API
--

-- E.g. http://example.com/book/1234?token=1337hAsk3ll

getRequestURL host apiKey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host = (\apiKey resource id -> getRequestURL host apiKey resource id)

exampleUrlBuilder = genHostRequestBuilder "http://example.com"

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

-- QC0502
genApiRequestBuilder' hostBuilder resource apiKey = (\id -> hostBuilder apiKey resource id)

-- Partial application: making closures simple

exampleUrlBuilder' = getRequestURL "http://example.com"

-- QC0503

myBuilder = getRequestURL "http://example.com" "1337hAsk3ll" "book"

--
-- Putting it all together
--

flipBinaryArgs f = \x y -> f y x      -- same as `flip`

-- QC0504

subtract2 = flip (-) 2

-- Q0501

ifEvenInc' = ifEven inc
ifEvenDouble = ifEven double
ifEvenSquare = ifEven square

-- Q0502

binaryPartialApplication f x = \y -> f x y

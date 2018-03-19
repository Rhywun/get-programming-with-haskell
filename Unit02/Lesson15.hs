module Lesson15 where

--
--
--  Ciphers for beginners: ROT13
--
--
data FourLetterAlphabet
  = L1
  | L2
  | L3
  | L4
  deriving (Eq, Show, Enum, Bounded)

-- Rotate an enum `enum` halfway around an alphabet of size `size`
-- E.g. rotN 4 L4 == L2
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN size enum = toEnum rotation -- E.g. L2
  where
    half = size `div` 2 -- E.g. 2
    offset = fromEnum enum + half -- E.g. 3 + 2 == 5
    rotation = offset `mod` size -- E.g. 5 `mod` 4 == 1

-- Char-specific rotN
rotChar :: Char -> Char
rotChar = rotN size
  where
    size = 1 + fromEnum (maxBound :: Char)

message = [L1, L3, L4, L1, L1, L2] :: [FourLetterAlphabet]

-- E.g. fourLetterEncoder message == [L3,L1,L2,L3,L3,L4]
--      fourLetterEncoder (fourLetterEncoder message) == message --> True
fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder = map rot4l
  where
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
    rot4l = rotN alphaSize

--
--
-- XOR: The magic of cryptography!
--
--

xor :: [Bool] -> [Bool] -> [Bool]
xor bs1 bs2 = map xorPair (zip bs1 bs2)
    where xorPair (b1, b2) = xorBool b1 b2
          xorBool b1 b2 = (b1 || b2) && not (b1 && b2)

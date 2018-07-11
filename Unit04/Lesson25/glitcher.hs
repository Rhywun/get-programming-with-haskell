import           System.Environment
import           System.Random
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched  <- randomReplaceByte imageFile
  let glitchedFileName = "glitched_" <> fileName
  BC.writeFile glitchedFileName glitched
  putStrLn "Done."

--
-- Pure code
--

-- Convert an Int to a valid ASCII byte
{-
intToChar 12345 -- 'i'
-}
intToChar :: Int -> Char
intToChar i = toEnum $ i `mod` 255

-- Convert an Int to a ByteString
{-
intToBC 12345 -- "i"
-}
intToBC :: Int -> BC.ByteString
intToBC i = BC.pack [intToChar i]

-- Insert charVal at loc in bytes
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = before <> newChar <> after
 where
  (before, rest) = BC.splitAt loc bytes
  after          = BC.drop 1 rest
  newChar        = intToBC charVal

-- Sort size bytes at start
sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
 where
  (before, rest ) = BC.splitAt start bytes
  (target, after) = BC.splitAt size rest
  changed         = BC.reverse (BC.sort target)

--
-- Impure code
--

-- Applies random numbers to `replaceByte`
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal  <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

-- QC3

-- Get a random char
{-
randomChar -- 'R'
randomChar -- '\RS'
randomChar -- '='
-}
randomChar :: IO Char
randomChar = do
  charVal <- randomRIO (0, 255)
  return (toEnum charVal)

randomSortSection :: BC.ByteString
randomSortSection = undefined

-- Giving up because I'm sick and tired of random breaking HIE

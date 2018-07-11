{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Encoding            as E
import           Data.Maybe

--
-- Working with book data
--

main :: IO ()
-- main = TIO.writeFile "books.html" (booksToHtml myBooks)
main = do
  marcData <- B.readFile "sample.mrc"
  let marcRecords = allRecords marcData
  print (length marcRecords)

--

type Author = T.Text
type Title = T.Text
type Html = T.Text

data Book = Book
  { author :: Author
  , title :: Title
  } deriving Show

bookToHtml :: Book -> Html
bookToHtml book = "<p>\n" <> titleInTags <> authorInTags <> "</p>\n"
 where
  titleInTags  = "<strong>" <> title book <> "</strong>\n"
  authorInTags = "<em>" <> author book <> "</em>\n"

book1 :: Book
book1 =
  Book {title = "The Conspiracy Against the Human Race", author = "Ligotti, Thomas"}

book2 :: Book
book2 = Book {title = "A Short History of Decay", author = "Cioran, Emil"}

book3 :: Book
book3 = Book {title = "The Tears of Eros", author = "Bataille, Georges"}

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat
  [ "<html>\n"
  , "<head><title>books</title>"
  , "<meta charset='utf-8'/>"
  , "</head>\n"
  , "<body>\n"
  , booksHtml
  , "\n</body>\n"
  , "</html>"
  ]
  where booksHtml = (mconcat . map bookToHtml) books

myBooks :: [Book]
myBooks = [book1, book2, book3]

--
-- Working with MARC records
--

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

rawToInt :: B.ByteString -> Int
rawToInt = (read . T.unpack . E.decodeUtf8)

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

-- Get the next record and the rest of the file
nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest stream = B.splitAt (getRecordLength stream) stream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords stream = if stream == B.empty then [] else next : allRecords rest
  where (next, rest) = nextAndRest stream

--

type MarcDirectoryRaw = B.ByteString

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 (B.drop 12 leader))

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
 where
  directoryLength = getDirectoryLength record
  afterLeader     = B.drop leaderLength record

--

type MarcDirectoryEntryRaw = B.ByteString

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == B.empty
  then []
  else nextEntry : splitDirectory restEntries
  where (nextEntry, restEntries) = B.splitAt dirEntryLength directory

--

data FieldMetadata = FieldMetadata
  { tag :: T.Text
  , fieldLength :: Int
  , fieldStart :: Int
  } deriving Show

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
 where
  (theTag, rest)        = B.splitAt 3 entry
  textTag               = E.decodeUtf8 theTag
  (rawLength, rawStart) = B.splitAt 4 rest
  theLength             = rawToInt rawLength
  theStart              = rawToInt rawStart

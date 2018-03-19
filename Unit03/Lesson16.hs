module Lesson16 where

--
--
-- Product types - combining types with “and”
--
--
data AuthorName = AuthorName
  { firstName :: String
  , lastName  :: String
  }

data BookV1 = BookV1
  { authorV1 :: AuthorName
  , isbnV1   :: String
  , titleV1  :: String
  , yearV1   :: Int
  , priceV1  :: Double
  }

--
--
-- Sum types - combining types with “or”
--
--
type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName
         LastName
  | NameWithMiddle FirstName
                   MiddleName
                   LastName
  | TwoInitialsWithLast Char
                        Char
                        LastName
  deriving (Show)

--
--
data Creator
  = AuthorCreator Author
  | ArtistCreator Artist
  deriving (Show)

newtype Author =
  Author Name
  deriving (Show)

data Artist
  = Person Name
  | Band String
  deriving (Show)

hpLovecraft =
  AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft")) :: Creator

--
--
-- Putting together your bookstore
--
--
data Book = Book
  { author    :: Creator
  , isbn      :: String
  , bookTitle :: String
  , bookYear  :: Int
  , bookPrice :: Double
  }

data VinylRecord = VinylRecord
  { artist      :: Creator
  , recordTitle :: String
  , recordYear  :: Int
  , recordPrice :: Double
  }

data CollectibleToy = CollectibleToy
  { name       :: String
  , descrption :: String
  , toyPrice   :: Double
  }

data StoreItem
  = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy

price :: StoreItem -> Double
price (BookItem book)     = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy)       = toyPrice toy

-- QC1603
madeBy :: StoreItem -> String
madeBy (BookItem book)     = show $ author book
madeBy (RecordItem record) = show $ artist record
madeBy (ToyItem toy)       = "Unknown Maker"

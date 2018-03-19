module Lesson16 where

import           Numeric

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
  | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book)     = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy)       = toyPrice toy
price (PamphletItem _)    = 0.0

-- QC1603
madeBy :: StoreItem -> String
madeBy (BookItem book)     = show $ author book
madeBy (RecordItem record) = show $ artist record
madeBy (ToyItem _)         = "Unknown Maker"

-- Q1601
data Pamphlet = Pamphlet
  { pamphletTitle      :: String
  , pamphletDescrption :: String
  , pamphletContact    :: String
  }

-- Q1602
type Radius = Double

type Height = Double

type Width = Double

data Shape
  = Circle Radius
  | Square Width
  | Rectangle Height
              Width

perimeter :: Shape -> Double
perimeter (Circle radius)          = 2 * pi * radius
perimeter (Square width)           = 4 * width
perimeter (Rectangle height width) = 2 * height + 2 * width

area :: Shape -> Double
area (Circle radius)          = pi * radius ^ 2
area (Square width)           = width ^ 2
area (Rectangle height width) = height * width

module Lesson16 where

-- Consider this

data BreakfastSide = Toast | Biscuit | Homefries | Fruit deriving Show
data BreakfastMeat = Sausage | Bacon | Ham deriving Show
data BreakfastMain = Egg | Pancake | Waffle deriving Show

data BreakfastSpecial
  = KidsSpecial BreakfastMain BreakfastSide
  | BasicSpecial BreakfastMain BreakfastMeat BreakfastSide
  | LumberjackSpecial BreakfastMain BreakfastMain
                      BreakfastMeat BreakfastMeat
                      BreakfastSide BreakfastSide BreakfastSide
  deriving Show

breakfast1 = KidsSpecial Waffle Homefries
breakfast2 = BasicSpecial Egg Sausage Toast

-- An invalid breakfast cannot be formed:
{-
breakfast3 = BasicSpecial Egg Sausage Bacon
-}

-- But an "incomplete" breakfast CAN be formed - it's just a function awaiting more args:
breakfast4 = LumberjackSpecial Egg Pancake

--
-- Product types - combining types with “and”
--

type FirstName = String
type LastName = String
type ISBN = String
type Title = String
type Year = Int
type Price = Double

newtype AuthorName' = FirstName LastName

data Book'' = AuthorName' ISBN Title Year Price

-- Or, using record sytax:

data Book' = Book'
  { authorV1 :: AuthorName
  , isbnV1   :: String
  , titleV1  :: String
  , yearV1   :: Int
  , priceV1  :: Double
  }

-- QC1

data AuthorName = AuthorName
  { firstName :: String
  , lastName  :: String
  }

-- QC2

data Car = Car

type Spoiler = String

data SportsCar = SportsCar Car Spoiler

--
-- Sum types - combining types with “or”
--

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  deriving (Show)

--

data Creator
  = AuthorCreator Author
  | ArtistCreator Artist
  deriving (Show)

newtype Author =  Author Name  deriving (Show)

data Artist  = Person Name  | Band String  deriving (Show)

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

--
-- Putting together your bookstore
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
price (BookItem     book  ) = bookPrice book
price (RecordItem   record) = recordPrice record
price (ToyItem      toy   ) = toyPrice toy
price (PamphletItem _     ) = 0.0

-- QC3

madeBy :: StoreItem -> String
madeBy (BookItem     book  ) = show $ author book
madeBy (RecordItem   record) = show $ artist record
madeBy (ToyItem      _     ) = "Unknown Maker"
madeBy (PamphletItem _     ) = undefined

-- Q1

data Pamphlet = Pamphlet
  { pamphletTitle      :: String
  , pamphletDescrption :: String
  , pamphletContact    :: String
  }

-- Q2

type Radius = Double

type Height = Double

type Width = Double

data Shape
  = Circle Radius
  | Square Width
  | Rectangle Height Width

perimeter :: Shape -> Double
perimeter (Circle radius         ) = 2 * pi * radius
perimeter (Square width          ) = 4 * width
perimeter (Rectangle height width) = 2 * height + 2 * width

area :: Shape -> Double
area (Circle radius         ) = pi * radius ^ (2 :: Int)
area (Square width          ) = width ^ (2 :: Int)
area (Rectangle height width) = height * width

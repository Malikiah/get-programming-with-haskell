module Main where

import Lib

-- this is a product type in Haskell very similar to structs in languages like C and its derivitives like GO.
-- struct book {
--     author_name author;
--     char *isbn;
--     char *title;
--     int year_published;
--     double price;
-- };
-- Haskell Product Type Example
-- data AuthorName = AuthorName String String
-- data Book = Author String String Int
-- OR you can define it like this in record syntax
data Book = Book {
    author :: AuthorName,
    isbn :: String,
    title :: String,
    year :: Int,
    bookPrice :: Double
}
-- QC 16.1 Rewrite AuthorName is record syntax
data AuthorName = AuthorName {
    firstName :: String,
    lastName :: String
}

-- QC 16.2
data Car = Car Int String String
data Spoiler = Spoiler String 
data SportsCar = SportsCar Car Spoiler

-- Sum Types are where you combine two types with an OR
-- These three are type synonyms
type FirstName = String
type MiddleName = String
type LastName = String

data Creator = AuthorCreator Author | ArtistCreator Artist

-- data AuthorCreator = Author Name
-- data ArtistCreator = Artist Artist

data Author = Author Name
data Artist = Person Name | Band String


data Name = Name FirstName LastName |
            NameWithMiddle FirstName MiddleName LastName |
            TwoInitialsWithLast Char Char String |
            FirstNameWithTwoInitials String Char Char 

hpLoveCraft :: Creator
hpLoveCraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data VinylRecord = VinylRecord {
    artist :: Creator,
    recordTitle :: String,
    recordYear :: Int,
    recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
    name :: String,
    description :: String,
    toyPrice :: Double
}

data StoreItem = BookItem Book | 
                 RecordItem VinylRecord |
                 ToyItem CollectibleToy |
                 PamphletItem Pamphlet
                 

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

-- QC 16.3
--madeBy :: StoreItem -> String
--madeBy (BookItem book) = show (author book)
--madeBy (RecordItem record) = show (artist record)
--madeBy _ = "unknown"

-- Q 16.1
data Pamphlet = Pamphlet {
    pamphletTitle :: String,
    pamphletDescription :: String,
    pamphletContact :: String,
    pamphletPrice :: Double
}

-- Q 16.2 
type Radius = Double
type Height = Double
type Width = Double

data Shape = Circle Radius |
             Square Height |
             Rectangle Height Width deriving Show



area :: Shape -> Double
area (Circle r) = pi*r^2
area (Square h) = h^2
area (Rectangle h w) = h*w

perimeter :: Shape -> Double
perimeter (Circle r) = pi*r*2
perimeter (Square h) = h*4
perimeter (Rectangle h w) = 2*h + 2*w



main :: IO ()
main = print "hello"

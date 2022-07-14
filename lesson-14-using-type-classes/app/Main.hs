module Main where

import Lib

-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 

-- Implementation of type class Show, In type classes 'functions' are called methods 
-- QC 14.1

instance Show SixSidedDie where
    show S1 = "I"
    show S2 = "II"
    show S3 = "III"
    show S4 = "IV"
    show S5 = "V"
    show S6 = "VI"
-- 
-- The need for polymorphism ( a finction that behaves differently depending on the type of data it is working with)
-- Polymorphism is an important concept for Haskell and OOP.


-- instance Eq SixSidedDie where
--     (==) S6 S6 = True
--     (==) S5 S5 = True
--     (==) S4 S4 = True
--     (==) S3 S3 = True
--     (==) S2 S2 = True
--     (==) S1 S1 = True
--     (==) _ _ = False

-- QC 14.2
-- properFraction

-- instance Ord SixSidedDie where
--     compare S6 S6 = EQ
--     compare S6 _ = GT
--     compare _ S6 = LT
--     compare S5 S5 = EQ
--     compare S5 _ = GT
--     compare _ S5 = LT
--     compare S4 S4 = EQ
--     compare S4 _ = GT
--     compare _ S4 = LT

-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show,Eq,Ord)

-- Creating an instance for a type class instead of deriving 
-- This can cause potential bugs
-- instance Enum SixSidedDie where
--     toEnum 0 = S1
--     toEnum 1 = S2
--     toEnum 2 = S3
--     toEnum 3 = S4
--     toEnum 4 = S5
--     toEnum 5 = S6
--     toEnum _ = error "No such value"
-- 
--     fromEnum S1 = 0
--     fromEnum S2 = 1
--     fromEnum S3 = 2
--     fromEnum S4 = 3
--     fromEnum S5 = 4
--     fromEnum S6 = 5

-- Instead derive Enum to avoid potential bugs unless you really need to
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum)

-- Q 14.1
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum)

instance Eq SixSidedDie where
    (==) num1 num2 = (fromEnum num1) == (fromEnum num2)
instance Ord SixSidedDie where
    compare num1 num2 = compare (fromEnum num1) (fromEnum num2)

-- Q 14.2
data FiveSidedDie = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Show,Eq,Enum)

-- class Die a where 
--     roll :: a -> FiveSidedDie
-- 
-- instance Die FiveSidedDie where

class (Eq a, Enum a) => Die a where
    roll :: Int -> a

instance Die FiveSidedDie where
    roll n = toEnum (n `mod` 5)


main :: IO ()
main = print "hello"

module Main where

import Lib

-- QC 13.1 Find the Type of the following : 
aList = ["cat","dog","mouse"]

-- QC 13.2 Why isn't division included in the list of functions needed for a Num?
-- division isn't included because it requires and produces a double. The Num Type Class is restricted to only using and producing an Int

class Describable a where 
    describe :: a -> String

-- QC 13.3
data Icecrream = Chocolate | Vanilla deriving (Show, Eq, Ord)
-- ghci> Chocolate > Vanilla
-- False
-- ghci> Vanilla > Chocolate
-- True


-- Q 13.1
-- Word is an integer that takes only positive numbers and more numbers than int

-- ghci> minBound :: Word
-- 0
-- ghci> maxBound :: Word
-- 18446744073709551615
-- 
-- ghci> minBound :: Int
-- -9223372036854775808
-- ghci> maxBound :: Int
-- 9223372036854775807

-- Q 13.3

cycleSucc :: (Bounded a, Enum a , Eq a) => a -> a
cycleSucc n = if n == maxBound
                then minBound
                else succ n

main :: IO ()
main = print "hello"

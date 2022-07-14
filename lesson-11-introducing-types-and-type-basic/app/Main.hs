module Main where

import Lib

-- QC 11.1
halve :: Integer -> Integer
halve x = x `div` 4

-- QC 11.2
printDouble :: Int -> String
printDouble x = show(x*2) 

-- QC 11.3
--makeAddress :: Int -> String -> String -> (Int,String,String)
-- makeAddress = (1,"h","hh")
-- QC 11.4
-- The reason it returns an [a] instead of a [b] is because it is not gurantee nor would we want it to be guranteed to return the same type that is put in.

-- Q11.1
-- What is the type signature of filter and how is it different than map?
-- the type signature of filter :: (a -> Bool ) -> [a] -> [a] , it is different than map because it returns a Bool and also is limited to returning a type that it was given.

-- Q11.2
myTail :: [a] -> [a]
myTail [] = []
myTail (x:xs) = xs

-- Q11.3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x

main :: IO ()
main = print "hello"

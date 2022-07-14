module Main where

import Lib

-- Q7.1
myTail [] = []
myTail (_:xs) = xs

-- Q7.2
myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
        where remainder = a `mod` b

myGCD2 a 0 = a
myGCD2 a b = myGCD2 b ( a `mod` b) 

-- This is what the recusion does until b = 0 then myGCD2 a 0 = a or can be represented as myGCD2 2 0 = 2
-- myGCD2 10 8 = myGCD2 8 ( 10 `mod` 8)
-- myGCD2 8 2 = myGCD2 2 ( 8 `mod` 2) 
-- myGCD2 2 0 = 2



main :: IO ()
main = print "hello"

-- Five Rules of Recursion
-- 1. Identify the End Goal
-- 2. Determine what happens when a goal is reached
-- 3. List all alertnate possibilities
-- 4. Dertmine Your "rinse and repeat" process
-- 5. Ensure That Each alternative moves you toward the goal.

module Main where

import Lib

-- QC8.1 myLength
-- Getting the lenght of a list by using pattern matching, get the tail of a list and add one for the length.
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 8.2 myTake 
myTake _ [] = []
myTake 0 _ = []

myTake n (x:xs) = x:rest
    where rest = myTake ( n - 1 ) xs

-- myTake 2 [1,2,3,4] = 1:[]:myTake ( 2 - 1 ) [2,3,4]
-- myTake 1 [2,3,4] = 1:2:[]:myTake ( 1 - 1 ) [3,4]
-- myTake 0 [3,4] = 1:2:[]:myTake ( 0 - 1 ) [4] or myTake 0 [3,4] = [1,2]

-- 8.3 myCycle
myCycle (first:rest) = first:myCycle (rest ++ [first])

-- 8.3.1 Ackermann function

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

-- following the rules of recursion quickly goes out of control with the ackermann function.
-- GHCI> :set +s
-- GHCI> ackermann 3 3
-- 61
-- (0.02 secs, 1,390,112 bytes)
-- ghci> ackermann 3 9
-- 4093
-- (7.37 secs, 3,906,596,224 bytes)
-- GHCI> ackermann 3 11
-- 16381
-- (118.78 secs, 62,605,031,496 bytes)

-- 8.3.2 The Collatz Conjecture
-- The collatz conjecture violates the 5th rule, we have no clear defined way of reaching our goal. 
-- These types of recursion can be used but should be tested is the way we wish to use the to avoid programs locking up,
-- but this is likely fine to use.
collatz 1 = 1

collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n*3 + 1)

-- Q8.1 myReverse

myReverse [] = []
myReverse (x:[]) = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

-- myReverse [1,2,3] = (myReverse [2,3] ++ [1])
-- myReverse [2,3] = (myReverse [3] ++ [2,1])
-- myReverse [3] = (myReverse [] ++ [3,2,1]) or represented as myReverse [] = [3,2,1] 

-- Q8.2 fastFib

fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 2
fastFib n1 n2 3 = n1 + n2
fastFib n1 n2 counter = fastFib ( n1 + n2 ) n1 ( counter - 1 )

fib n = fastFib 1 1 n

-- fib 1000 = fastFib 1 1 1000
-- fastFib 1 1 1000 = fastFib ( 1 + 1 ) 1 ( counter - 1000 )
-- fastFib 2 1 999 = fastFib ( 2 + 1 ) 1 ( counter - 999 )
-- fastFib ( 3 + 1 ) 1 998 = fastFib ( 4 + 1) 1 ( counter - 998 ) 



main :: IO ()
main = print "hello"

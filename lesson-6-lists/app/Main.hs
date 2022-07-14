module Main where

import Lib

-- Q6.1
repeatForever n = cycle [n]

-- Q6.2
subseq start end word = take difference $ drop start word
    where difference = end - start

-- Q6.3
inFirstHalf testList word = word `elem` halvedList
    where lengthList = (length testList) `div` 2
          halvedList = reverse $ drop lengthList $ reverse testList

main :: IO ()
main = print "hello"

module Main where

import Lib

-- 9.2
myMap f (x:xs) = (f x):myMap f xs
-- f x = x + 1
-- myMap f [1,2,3] = (f 1):myMap f [2,3]
-- myMap f [2,3] = (f 2):myMap f [3]
-- myMap f [3] = (f 3):myMap f []
-- myMap f [3] = (f 1):(f 2):(f 3):myMap f []
-- 
-- 2:3:4:[]

test (x:xs) = x == 'a'
-- 9.3
myFilter test [] = []
myFilter test (x:xs) = if test x
                        then x:myFilter test xs
                        else myFilter test xs

outputMyFilter = myFilter test ["apple", "banana", "avocado"]
-- ["apple","avocado"]

-- QC9.1

remove test [] = []

remove test (x:xs) = if test x
                        then remove test xs
                        else x:remove test xs

outputRemove = remove test ["apple", "banana", "avocado"]
-- ["banana"]

-- QC9.2
myProduct xs = foldl (*) 1 xs 



-- 9.5 myFoldl

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x

-- 9.6 myFoldr
myFolder f init [] = init
myFoldr f init (x:xs) = f x rightResult
    where rightResult = myFoldr f init xs

-- Q9.1
myElem _ [] = False

myElem value myList = (length filteredWord) /= 0
    where filteredWord = filter ( == value ) myList

-- Q9.2

-- isSpace = (\x -> (x == ' '))
myPalindrome text = if noSpaceText == reverse noSpaceText
                        then return True
                        else return False
    where noSpaceText = filter (/= ' ') text

harmonic 0 c s = s
harmonic n c s = harmonic (n - 1) counter (1 / counter + s)
    where counter = c + 1

harmonicFromBook :: (Enum a, Fractional a) => Int -> a
harmonicFromBook n = sum (take n seriesValues)
    where seriesPairs = zip (cycle [1.0]) [1.0,2.0 ..]
          seriesValues = map (\pair -> (fst pair)/(snd pair)) seriesPairs

main :: IO ()
main = print "hello"

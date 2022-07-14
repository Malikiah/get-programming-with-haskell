module Main where

import Lib

-- QC 17.1
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

-- QC 17.2
-- Division doesn't always return an Int it can return a Double.

data Color = Red |
             Yellow |
             Blue |
             Green |
             Purple |
             Orange |
             Brown deriving (Show,Eq)

instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple    
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    -- here we are using guards to change the output based on a condition otherwise brown
    (<>) a b | a == b = a
             | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
             | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
             | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
             | otherwise = Brown

-- QC 17.3
-- Yes because addition is associative.


-- QC 17.4
-- 1,because x * 1 = x

-- monoid consists of three pieces mappend mempty mconcat
-- mappend is equivalent to (++)
-- mempty is equivalent to []
-- Monoid Laws
-- The first is that mappend mempty x is x. Remembering that mappend is the same as (++), and mempty is [] for lists, this intuitively means that
-- [] ++ [1,2,3] = [1,2,3]
-- The second is just the first with the order reversed: mappend x mempty is x. In list form this is
-- [1,2,3] ++ [] = [1,2,3]
-- The third is that mappend x (mapped y z) = mappened (mappened x y) z. this is just associativity (associativity is definted in mathmatics as "the indepence of the grouping of elements" meaning the grouping of the elements doesn't matter )
-- (a + b) + c = a + (b + c)
-- Because this is a Semigroup law, then if mappend is already implemted as <>, this law can be assumed because it's required by the Semigroup laws.
-- The fourth is just our deinition of mconcat :
-- mconcat = foldr mappend mempty

-- monoids building probabilty tables

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event,"|", show prob, "\n"]

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
    where totalProbs = sum probs
          normalizedProbs = map (\x -> x/totalProbs) probs

instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

-- Cartesian Product
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
    where combiner = (\x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where 
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
        where newEvents = combineEvents e1 e2
              newProbs = combineProbs p1 p2 

instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

coin :: PTable
coin = createPTable ["heads", "tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1,0.2,0.7]



main :: IO ()
main = print "hello"

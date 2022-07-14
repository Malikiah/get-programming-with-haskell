module Main where

import Lib
import System.Environment
import Control.Monad

myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n func = mapM (\_ -> func) [1 .. n]

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args

    let linesToRead = if length args > 0
                      then read (head args)
                      else 0 :: Int

    numbers <- myReplicateM linesToRead getLine
    let ints = map read numbers :: [Int]
    print (sum ints)

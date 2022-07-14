module SumLazy where
-- Lazy evaluation getting command line inputs 
main :: IO ()

toInts :: String -> [Int]
toInts = map read . lines

main = do
    userInput <- getContents
    let numbers = toInts userInput
    print (sum numbers)
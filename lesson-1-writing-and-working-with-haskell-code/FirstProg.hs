module FirstProg where

toPart recepient = "Dear" ++ recepient ++ ",\n"
bodyPart bookTitle = "Thanks for buying" ++ bookTitle ++ "\n"
fromPart author = "Thanks, \n" ++ author

main :: IO()
main = print "hello"


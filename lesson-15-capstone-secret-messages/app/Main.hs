module Main where

import Lib

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
        -- fromEnum takes the the item in a predefined data type and returns its place in the list L1 = 0 L2 = 1 L3 = 2 L4 = 3
          offset = fromEnum c + halfAlphabet
          rotation = offset `mod` alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)


rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
  where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

message :: [FourLetterAlphabet]
message = [L1,L3,L4,L1,L1,L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
  where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot4l = rotN alphaSize


rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where halfN = n `div` 2
        offset = if even n
                  then fromEnum c + halfN
                  else 1 + fromEnum c + halfN
        rotation = offset `mod` n

rotEncoder :: String -> String
rotEncoder text = map rotChar text 
  where alphaSize = 1 + fromEnum (maxBound :: Char)
        rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text 
  where alphaSize = 1 + fromEnum (maxBound :: Char)
        rotCharDecoder = rotN alphaSize       

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool,Bool) -> Bool
xorPair (v1,v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
                  then False : intToBits' nextVal
                  else True : intToBits' nextVal
  where remainder = n `mod` 2
        nextVal = n `div` 2
      
maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where reversedBits = reverse (intToBits' n) 
        missingBits = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
  where size = length bits
        indices = [size-1, size-2 .. 0]
        trueLocations = filter (\x -> fst x == True)
                        (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair -> (fst pair) `xor` (snd pair)) (zip padBits plaintextBits)
  where padBits = map charToBits pad
        plaintextBits = map charToBits plaintext



applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad 

class Cipher a where 
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot


instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a*seed + b) `mod` maxNumber

-- extending the execercise
-- use the PRNG to create a StreamCipher type that can be an instance of the Cipher class.


createSeed :: String -> Int
-- this way of generating a seed is not condusive to security just used as a way to get a value to place 
-- inside of rPRNG since the seed is derived from the text. This is the correct idea in that you would want a different
-- seed for each encryption since an OTP can be cracked if there are multiple things encrypted with the same OTP.
-- also this utilizes the functions that were made in the lesson and to show my understanding of these function
-- it is good to implement them.
createSeed (x:xs) = foldl (+) (bitsToInt $ charToBits x) (map bitsToInt ( map charToBits xs))

seedlessPRNG :: String -> Int
-- This is the function used for the OTP if there was no seed provided likely in the case of first encryption.
seedlessPRNG text = pRandomNumber
 -- maxNumber sets the max size of of the number based on the size of the given string, and prepends a 1 to make it an actual number then makes it an Int by using read.
 where maxNumber = read (concat $ map show (1:zeros)) :: Int
      -- zeros creates a string string of zeros using an infinite cycle of zero, then uses take based on the length of the string/text to be encrypted.
       zeros = take (length text) $ cycle [0..0]
       -- seed is made from a function called createSeed that creates a seed based on the bits of the string.
       seed = createSeed text
       -- prandomNumber uses prng function from the book to create a psudo random number based on the the strings length and the strings bits.
       pRandomNumber = prng 1337 7 maxNumber (seed * 10000)

-- seededPRNG does the same thing as seedless just without the seeded creation. This is to be used if you have a seed that you want to encrypt something with
-- or you want to decrypt and you know the seed that was generated from the seedlessPRNG.
seededPRNG seed text = pRandomNumber
 where maxNumber = read $ concat $ map show (1:zeros) :: Int
       zeros = take (length text) $ cycle [0..0]
       pRandomNumber = prng 1337 7 maxNumber (seed * 10000)

encodeStreamCipher text = applyOTP (show $ seedlessPRNG text) text
decodeStreamCipher seed text = applyOTP (show $ seededPRNG seed text) text

data StreamCipher = StreamCipher Int

instance Cipher StreamCipher where
 encode (StreamCipher seed) text = encodeStreamCipher text
 decode (StreamCipher seed) text = decodeStreamCipher seed text


main :: IO ()
main = print "hello"
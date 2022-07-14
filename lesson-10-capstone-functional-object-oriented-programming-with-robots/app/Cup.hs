module Cup where

-- This is how you make a constructor for an Object in haskell
cup flOz = \message -> message flOz

-- This is how you define an object using the cup constructor
coffeeCup = cup 12

-- This is how you create an accessor for your objects, you 
getOz aCup = aCup (\flOz -> flOz)

-- This is how we would modify an objects state when acted up and protect it from going out of range/drinking more coffee than the cup can supply.
drink aCup ozDrank = if ozDiff >= 0
                        then cup ozDiff
                        else cup 0
    where flOz = getOz aCup
          ozDiff = flOz - ozDrank

afterASip = drink coffeeCup 1
afterTwoSips = drink afterASip 1
afterGulp = drink afterTwoSips 4
afterBigGulp = drink coffeeCup 20
afterManySips = foldl drink coffeeCup [1,1,1,1,1]

isEmpty aCup = getOz aCup == 0


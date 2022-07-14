module Robot where

-- robot constructor with pattern matching
robot (name,attack,hp) = \message -> message (name,attack,hp)
-- pattern matching helper functions
name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp

killerRobot = robot ("Kill3r",25,200)
gentleGiant = robot ("Mr. Friendly",10,300)

-- Creation of our getters
getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

-- Creation of our setters
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

-- Prototype based OOP; these objects are created through mofigying a prototypical object.
nicerRobot = setName killerRobot "kitty"
gentlerRobot = setAttack killerRobot 5
softerRobot = setHP killerRobot 50

printRobot aRobot = aRobot (\(n,a,h) -> n ++ " attack:" ++ (show a ) ++ " hp:" ++ (show h))

damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h-attackDamage))

fight aRobot defender = damage defender attack
  where attack = if (getHP aRobot) > 10
                 then getAttack aRobot
                 else 0


gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

-- GHCI> printRobot gentleGiantRound3
-- "Mr. Friendly attack:10 hp:225"
-- GHCI> printRobot killerRobotRound3
-- "Kill3r attack:25 hp:170"

-- threeRoundFight killerRobot gentleGiant rounds = if rounds == 0
--                             then if r1HP > r2HP
--                                     then killerRobot
--                                     else gentleGiant
--                             else threeRoundFight (fight gentleGiant killerRobot) (fight gentleGiant killerRobot) (rounds - 1)
--     where r1HP = getHP killerRobot
--           r2HP = getHP gentleGiant


  

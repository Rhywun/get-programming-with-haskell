module Lesson10 where

--
-- An object with one property: a cup of coffee
--

cup oz = \message -> message oz

coffeeCup = cup 12

{-
getOz coffeeCup -- 12
-}
getOz aCup = aCup (\oz -> oz)

drink aCup ozDrank = if ozDiff >= 0 then cup ozDiff else cup 0
 where
  oz     = getOz aCup
  ozDiff = oz - ozDrank

{-
getOz afterASip -- 11
-}
afterASip = drink coffeeCup 1

{-
getOz afterBigGulp -- 0
-}
afterBigGulp = drink coffeeCup 20

isEmpty aCup = getOz aCup == 0

{-
getOz afterManySips -- 7
-}
afterManySips = foldl drink coffeeCup [1, 1, 1, 1, 1]

--
-- A more complex object: let’s build fighting robots!
--

robot (name, attack, hp) = \message -> message (name, attack, hp)

killerRobot = robot ("Kill3r", 25, 200)
name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

nicerRobot = setName killerRobot "kitty"
gentlerRobot = setAttack killerRobot 5
softerRobot = setHP killerRobot 50

printRobot aRobot =
  aRobot (\(n, a, h) -> n ++ " attack:" ++ show a ++ " hp:" ++ show h)

damage aRobot attackDamage =
  aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10 then getAttack aRobot else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

--
-- Why stateless programming matters
--

fastRobot = robot ("speedy", 15, 40)
slowRobot = robot ("slowpoke", 20, 30)

fastRobotRound3 = fight slowRobotRound3 fastRobotRound2
fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
fastRobotRound1 = fight slowRobotRound1 fastRobot
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
slowRobotRound1 = fight fastRobot slowRobot

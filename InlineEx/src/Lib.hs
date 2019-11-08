-- 23
module Lib where

import System.Random

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--newtype State s a =
--    State { runState :: (s -> (a, s)) }

data Die =
         DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix
         | BadDie
    deriving (Eq, Show)

dieToInt :: Die -> Int
dieToInt d
    | d == DieOne   = 1
    | d == DieTwo   = 2
    | d == DieThree = 3
    | d == DieFour  = 4
    | d == DieFive  = 5
    | d == DieSix   = 6

intToDie :: Int -> Die
intToDie n
    | n == 1 = DieOne
    | n == 2 = DieTwo
    | n == 3 = DieThree
    | n == 4 = DieFour
    | n == 5 = DieFive
    | n == 6 = DieSix
    | otherwise = BadDie

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s0 = mkStdGen 0
      (d1, s1) = randomR (1,6) s0
      (d2, s2) = randomR (1,6) s1
      (d3, _)  = randomR (1,6) s2
  (intToDie d1, intToDie d2, intToDie d3)

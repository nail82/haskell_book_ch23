module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import Lib

{--
newtype State s a =
  State { runState :: (s -> (a, s)) }
--}

rollDie :: State StdGen Die
rollDie = state $ do
            (n, s) <- randomR (1,6)
            return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1,6))

rollThreeDie' :: State StdGen (Die, Die, Die)
rollThreeDie' = liftA3 (,,) rollDie rollDie rollDie

-- This one doesn't work correctly.
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where
      go :: Int -> Int -> StdGen -> Int
      go cnt sm gen
          | sm >= 20 = cnt
          | otherwise = let (n, nextGen) = randomR (1,6) gen
                        in go (cnt + 1) (sm + n) nextGen

rollsToGetToN :: Int -> StdGen -> Int
rollsToGetToN n g = go 0 0 g
    where
      go :: Int -> Int -> StdGen -> Int
      go cnt sm gen
         | sm >= n = cnt
         | otherwise = let (m, nextGen) = randomR (1,6) gen
                       in go (cnt + 1) (sm + m) nextGen


rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = (length ds, ds)
    where ds = go 0 g []
          go :: Int -> StdGen -> [Int] -> [Die]
          go sm gen xs
             | sm >= n = reverse $ fmap intToDie xs
             | otherwise = let (m, nextGen) = randomR (1,6) gen
                           in go (sm + m) nextGen (m : xs)

-- Playing around with repeat
repeatN :: Int -> a -> [a]
repeatN n a = go n a 0 []
    where
      go :: Int -> a -> Int -> [a] -> [a]
      go n' a' cnt xs
          | cnt >= n' = xs
          | otherwise = go n' a' (cnt + 1) (a' : xs)

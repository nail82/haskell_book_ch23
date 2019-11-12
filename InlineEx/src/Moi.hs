{-# LANGUAGE InstanceSigs #-}

module Moi where

newtype Moi s a =
    Moi { runMoi :: (s -> (s, a)) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f m = Moi (\s -> (s, f $ snd (runMoi m s)))

fakeState :: Int -> (Int, Int)
fakeState i = (i+1, i)

-- Have:
-- s -> (s, a)
-- a -> b

-- Moi s b
-- Moi { runMoi :: (s -> (s, b)) }

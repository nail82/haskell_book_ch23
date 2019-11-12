{-# LANGUAGE InstanceSigs #-}

module Moi where

-- Note the order of the function return value.
-- There are opposite the order of the type args.
-- That tripped me up along with the way the problem
-- was written in the book.
newtype Moi s a =
    Moi { runMoi :: (s -> (a, s)) }

-- Fmapping modifies the outcome variable, a, not the state.
instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f m = Moi (\s -> (f $ fst (runMoi m s), s))
    -- The book had the problem setup as
    -- fmap f (Moi s) = undefined

fakeState :: Int -> (Int, Int)
fakeState i = (i, i+1)

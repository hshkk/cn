module Control.Recursive.Subset where

import Control.Memoization (M)
import Control.Monad.State (get, modify, evalState)

import qualified Data.Map as M

s :: (Ord a,Num a) => [a] -> a -> Bool
s _      0 = True
s []     _ = False
s [x]    t = x==t
s (x:xs) t = s xs t || x<=t && s xs (t-x)

s' :: (Ord a,Num a) => [a] -> a -> Int -> M (Int,a) Bool
s' _      0 _ = return True
s' []     _ _ = return False
s' [x]    t _ = return (x==t)
s' (x:xs) t i =
    get >>= \st ->
    case M.lookup (i,t) st of
        Just b  -> return b
        Nothing ->
            s' xs t (i+1) >>= \b ->
            if b
            then modify (M.insert (i+1,t) b) >>
                 return b
            else if x<=t
                 then s' xs (t-x) (i+1) >>= \c ->
                      modify (M.insert (i+1,t-x) c) >>
                      return c
                 else modify (M.insert (i+1,t-x) False) >>
                      return False

rs' :: (Ord a,Num a) => [a] -> a -> Bool
rs' xs t = evalState (s' xs t 0) M.empty

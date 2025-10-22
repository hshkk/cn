module Control.Recursive.Levenshtein where

import Control.Memoization (M)
import Control.Monad.State (get, modify, evalState)
import qualified Data.Map as M

d :: String -> String -> Int
d xs       []  = length xs
d []       ys  = length ys
d l@(x:xs) r@(y:ys)
    | x==y      = d xs ys
    | otherwise =
        minimum [ d xs r
                , d l  ys
                , d xs ys ] + 1

d' :: Int -> Int -> String -> String -> M (Int,Int) Int
d' i 0  _ _ = return i
d' 0  j _ _ = return j
d' i  j  l r =
    get >>= \st ->
    case M.lookup (i,j) st of
        Just n  -> return n
        Nothing -> 
            (let li = i-1; lj = j-1 in
            if l !! li == r !! lj 
            then d' li lj l r
            else d' li j  l r >>= \del ->
                 d' i  lj l r >>= \ins ->
                 d' li lj l r >>= \sub ->
                 return (minimum [del,ins,sub] + 1))
            >>= \z -> 
            modify (M.insert (i,j) z) >> 
            return z

rd' :: String -> String -> Int
rd' x y = evalState (d' (length x) (length y) x y) M.empty
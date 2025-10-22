module Control.Recursive.Levenshtein where

import Control.Memoization (M')
import Control.Monad.State (get, modify, evalState)
import Data.Bifunctor      (first)

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

type String2 = (String,String)

d' :: Int -> Int -> M' (M.Map (Int,Int) Int,String2) Int
d' i 0 = return i
d' 0 j = return j
d' i j =
    get >>= \(m,(l,r)) ->
    case M.lookup (i,j) m of
        Just n  -> return n
        Nothing ->
            (let li = i-1; lj = j-1 in
            if l !! li == r !! lj
            then d' li lj
            else d' li j  >>= \del ->
                 d' i  lj >>= \ins ->
                 d' li lj >>= \sub ->
                 return (minimum [del,ins,sub] + 1))
            >>= \z ->
            modify (first (M.insert (i, j) z)) >>
            return z

rd' :: String -> String -> Int
rd' x y = evalState (d' (length x) (length y)) (M.empty,(x,y))
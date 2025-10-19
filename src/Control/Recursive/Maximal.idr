module Control.Recursive.Maximal

import Data.List

m : Ord a => (xys : List (a, a)) -> (n : a) -> Nat
m []         _ = 0
m ((x,y)::r) n =
    if y > n
    then 1 + m r y
    else m r n

rm : (xys : List (Nat, Nat)) -> Nat
rm xys = let xys'=reverse (sort xys) in m xys' 0